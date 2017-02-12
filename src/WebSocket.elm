effect module WebSocket where { command = MyCmd, subscription = MySub } exposing
  ( send
  , listen
  )

{-| Web sockets make it cheaper to talk to your servers.

Connecting to a server takes some time, so with web sockets, you make that
connection once and then keep using. The major benefits of this are:

  1. It faster to send messages. No need to do a bunch of work for every single
  message.

  2. The server can push messages to you. With normal HTTP you would have to
  keep *asking* for changes, but a web socket, the server can talk to you
  whenever it wants. This means there is less unnecessary network traffic.

The API here attempts to cover the typical usage scenarios, but if you need
many unique connections to the same endpoint, you need a different library.

# Web Sockets
@docs listen, send

-}

import Dict
import Process
import Task exposing (Task)
import WebSocket.LowLevel as WS



-- COMMANDS


type MyCmd msg
  = Send String String


{-| Send a message to a particular address. You might say something like this:

    send "ws://echo.websocket.org" "Hello!"

**Note:** It is important that you are also subscribed to this address with
`listen`. If you are not, the web socket will be created to send one message
and then closed. Not good!
-}
send : String -> String -> Cmd msg
send url message =
  command (Send url message)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Send url msg) =
  Send url msg



-- SUBSCRIPTIONS


type MySub msg
  = MySub String String (String -> msg)


{-| Subscribe to any incoming messages on a websocket. You might say something
like this:

    type Msg = Echo String | ...

    subscriptions model =
      listen "ws://echo.websocket.org" Echo

**Note:** If the connection goes down, the effect manager tries to reconnect
with an exponential backoff strategy.
-}
listen : String -> (String -> msg) -> Sub msg
listen url tagger =
  subscription (MySub "listen" url tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    MySub category url tagger ->
      MySub category url (tagger >> func)



-- MANAGER


type alias State msg =
  { sockets : SocketsDict
  , subs : SubsDict msg
  }


type alias SocketsDict =
  Dict.Dict String Connection


type alias SubsDict msg =
  Dict.Dict String (Dict.Dict String (String -> msg))


type Connection
  = Opening Int Process.Id
  | Connected WS.WebSocket


init : Task Never (State msg)
init =
  Task.succeed (State Dict.empty Dict.empty)



-- HANDLE APP MESSAGES


(&>) t1 t2 =
  Task.andThen (\_ -> t2) t1


onEffects
  : Platform.Router msg Msg
  -> List (MyCmd msg)
  -> List (MySub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router cmds subs state =
  let
    newSubs =
      buildSubDict subs Dict.empty

    newEntries =
      buildEntriesDict subs Dict.empty

    leftStep category _ getNewSockets =
      getNewSockets
        |> Task.andThen (\newSockets -> attemptOpen router 0 category
        |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Opening 0 pid) newSockets)))

    bothStep category _ connection getNewSockets =
      Task.map (Dict.insert category connection) getNewSockets

    rightStep category connection getNewSockets =
      closeConnection connection &> getNewSockets

    collectNewSockets =
      Dict.merge leftStep bothStep rightStep newEntries state.sockets (Task.succeed Dict.empty)
  in
    collectNewSockets
      |> Task.andThen (\newSockets -> Task.succeed (State newSockets newSubs))



buildSubDict : List (MySub msg) -> SubsDict msg -> SubsDict msg
buildSubDict subs dict =
  case subs of
    [] ->
      dict

    MySub category name tagger :: rest ->
      buildSubDict rest (Dict.update category (set (name, tagger)) dict)


buildEntriesDict : List (MySub msg) -> Dict.Dict String (List a) -> Dict.Dict String (List a)
buildEntriesDict subs dict =
  case subs of
    [] ->
      dict

    MySub category name tagger :: rest ->
      case category of
        "listen" ->
          buildEntriesDict rest (Dict.update name (Just << Maybe.withDefault []) dict)

        _ ->
          buildEntriesDict rest dict


set : (comparable, b) -> Maybe (Dict.Dict comparable b) -> Maybe (Dict.Dict comparable b)
set value maybeDict =
  case maybeDict of
    Nothing ->
      Just (Dict.fromList [value])

    Just list ->
      Just (Dict.fromList [value])



-- HANDLE SELF MESSAGES


type Msg
  = Receive String String
  | Die String
  | GoodOpen String WS.WebSocket
  | BadOpen String


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router selfMsg state =
  case selfMsg of
    Receive name str ->
      let
        sends =
          Dict.get "listen" state.subs
            |> Maybe.withDefault Dict.empty
            |> Dict.toList
            |> List.map (\(_, tagger) -> Platform.sendToApp router (tagger str))
      in
        Task.sequence sends &> Task.succeed state

    Die name ->
      case Dict.get name state.sockets of
        Nothing ->
          Task.succeed state

        Just _ ->
          attemptOpen router 0 name
            |> Task.andThen (\pid -> Task.succeed (updateSocket name (Opening 0 pid) state))

    GoodOpen name socket ->
        Task.succeed state

    BadOpen name ->
      case Dict.get name state.sockets of
        Nothing ->
          Task.succeed state

        Just (Opening n _) ->
          attemptOpen router (n + 1) name
            |> Task.andThen (\pid -> Task.succeed (updateSocket name (Opening (n + 1) pid) state))

        Just (Connected _) ->
          Task.succeed state


updateSocket : String -> Connection -> State msg -> State msg
updateSocket name connection state =
  { state | sockets = Dict.insert name connection state.sockets }



-- OPENING WEBSOCKETS WITH EXPONENTIAL BACKOFF


attemptOpen : Platform.Router msg Msg -> Int -> String -> Task x Process.Id
attemptOpen router backoff name =
  let
    goodOpen ws =
      Platform.sendToSelf router (GoodOpen name ws)

    badOpen _ =
      Platform.sendToSelf router (BadOpen name)

    actuallyAttemptOpen =
      open name router
        |> Task.andThen goodOpen
        |> Task.onError badOpen
  in
    Process.spawn (after backoff &> actuallyAttemptOpen)


open : String -> Platform.Router msg Msg -> Task WS.BadOpen WS.WebSocket
open name router =
  WS.open name
    { onMessage = \_ msg -> Platform.sendToSelf router (Receive name msg)
    , onClose = \details -> Platform.sendToSelf router (Die name)
    }


after : Int -> Task x ()
after backoff =
  if backoff < 1 then
    Task.succeed ()

  else
    Process.sleep (toFloat (10 * 2 ^ backoff))



-- CLOSE CONNECTIONS


closeConnection : Connection -> Task x ()
closeConnection connection =
  case connection of
    Opening _ pid ->
      Process.kill pid

    Connected socket ->
      WS.close socket
