effect module WebSocket where { command = MyCmd, subscription = MySub } exposing
  ( send
  , listen
  , connect
  , onOpen
  , onClose
  , Error(..)
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
@docs connect, listen, send, onOpen, onClose, Error

-}

import Dict
import Process
import Task exposing (Task)
import WebSocket.LowLevel as WS



-- COMMANDS


type MyCmd msg
  = Send String String
  | Connect String


-- ERRORS


{-| The `connect` and `send` functions may fail for a variety of reasons.
In each case, the browser will provide a string with additional information.
-}
type Error
    = ConnectFailed
    | SendFailed



{-| Send a message to a particular address. You might say something like this:

    send "ws://echo.websocket.org" "Hello!"

**Note:** It is important that you are also subscribed to this address with
`listen`. If you are not, the web socket will be created to send one message
and then closed. Not good!
-}
send : String -> String -> Cmd msg
send url message =
  command (Send url message)


{-| Attempt to connect to a particular address. You might say something like this:

    connect "ws://echo.websocket.org"

**Note:** It is important that you are also subscribed to this address with
`listen` if you want to handle messages from the connection!
-}
connect : String -> Cmd msg
connect url =
  command (Connect url)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ cmd =
  case cmd of
    Send url msg ->
      Send url msg

    Connect url ->
      Connect url


-- SUBSCRIPTIONS


type MySub msg
  = MySub String String (String -> msg)


{-| Subscribe to any incoming messages on a websocket. You might say something
like this:

    type Msg = Echo String | ...

    subscriptions model =
      listen "ws://echo.websocket.org" Echo

-}
listen : String -> (String -> msg) -> Sub msg
listen url tagger =
  subscription (MySub "listen" url tagger)


{-| Subscribe to websocket open events. You might say something
like this:

    type Msg = WsOpened String | ...

    subscriptions model =
      onOpen WsOpened
-}
onOpen : (String -> msg) -> Sub msg
onOpen tagger =
  subscription (MySub "onOpen" "" tagger)


{-| Subscribe to websocket close events. You might say something
like this:

    type Msg = WsClosed String | ...

    subscriptions model =
      onClose WsClosed
-}
onClose : (String -> msg) -> Sub msg
onClose tagger =
  subscription (MySub "onClose" "" tagger)


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
  = Opening Process.Id
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

    leftStep name _ getNewSockets =
      getNewSockets

    bothStep name _ connection getNewSockets =
      Task.map (Dict.insert name connection) getNewSockets

    rightStep name connection getNewSockets =
      closeConnection connection &> getNewSockets
  in
    Dict.merge leftStep bothStep rightStep newEntries state.sockets (Task.succeed Dict.empty)
      |> Task.andThen (\newSockets -> cmdHelp router cmds newSockets)
      |> Task.andThen (\newSockets -> Task.succeed (State newSockets newSubs))


cmdHelp : Platform.Router msg Msg -> List (MyCmd msg) -> SocketsDict -> Task Never SocketsDict
cmdHelp router cmds socketsDict =
  case cmds of
    [] ->
      Task.succeed socketsDict

    Send name msg :: rest ->
      case Dict.get name socketsDict of
        Just (Connected socket) ->
          WS.send socket msg
            &> cmdHelp router rest socketsDict

        _ ->
          Task.succeed socketsDict
          --Task.fail SendFailed -- Not connected

    Connect name :: rest ->
      case Dict.get name socketsDict of
        Just (Connected _) ->
          Task.succeed socketsDict
          --Task.fail ConnectFailed -- already connected

        _ ->
            attemptOpen router name
            |> Task.andThen (\pid -> Task.succeed (Dict.insert name (Opening pid) socketsDict))



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
  | Open String WS.WebSocket


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

        Just conn ->
          let
            sends =
              case conn of
                Connected _ ->
                  Dict.get "onClose" state.subs
                    |> Maybe.withDefault Dict.empty
                    |> Dict.toList
                    |> List.map (\(_, tagger) -> Platform.sendToApp router (tagger name))

                Opening _ -> -- Don't report close events if we never actually connected
                    []
          in
            Task.sequence sends &> Task.succeed state
              |> Task.andThen (\_ -> Task.succeed (removeSocket name state))

    Open name socket ->
      let
        sends =
          Dict.get "onOpen" state.subs
            |> Maybe.withDefault Dict.empty
            |> Dict.toList
            |> List.map (\(_, tagger) -> Platform.sendToApp router (tagger name))
      in
        Task.sequence sends &> Task.succeed state
            |> Task.andThen (\_ -> Task.succeed (updateSocket name (Connected socket) state))


removeSocket : String -> State msg -> State msg
removeSocket name state =
  { state | sockets = Dict.remove name state.sockets }


updateSocket : String -> Connection -> State msg -> State msg
updateSocket name connection state =
  { state | sockets = Dict.insert name connection state.sockets }


attemptOpen : Platform.Router msg Msg -> String -> Task x Process.Id
attemptOpen router name =
  let
    goodOpen ws =
      Platform.sendToSelf router (Open name ws)

    badOpen _ =
      Platform.sendToSelf router (Die name)

    actuallyAttemptOpen =
      open name router
        |> Task.andThen goodOpen
        |> Task.onError badOpen
  in
    Process.spawn actuallyAttemptOpen


open : String -> Platform.Router msg Msg -> Task WS.BadOpen WS.WebSocket
open name router =
  WS.open name
    { onMessage = \_ msg -> Platform.sendToSelf router (Receive name msg)
    , onClose = \details -> Platform.sendToSelf router (Die name)
    }



-- CLOSE CONNECTIONS


closeConnection : Connection -> Task x ()
closeConnection connection =
  case connection of
    Opening pid ->
      Process.kill pid

    Connected socket ->
      WS.close socket
