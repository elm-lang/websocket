effect module WebSocket { command = MyCmd, subscription = MySub }
  ( send
  , listen
  , keepAlive
  )
  where


import Dict
import Task exposing (Task)
import Time exposing (Time)
import WebSocket.LowLevel as WS



-- COMMANDS


type MyCmd msg
  = Send String String


send : String -> String -> Cmd msg
send url message =
  command (Send url message)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ (Send url msg) =
  Send url msg



-- SUBSCRIPTIONS


type MySub msg
  = Listen String (String -> msg)
  | KeepAlive String


{-| Subscribe to any incoming messages on a websocket. If the connection goes
down, the effect manager tries to reconnect with an exponential backoff
strategy.
-}
listen : String -> (String -> msg) -> Sub msg
listen url tagger =
  subscription (Listen url tagger)


{-| Keep a connection alive, but do not report any messages. This is useful
for keeping a connection open for when you only need to `send` messages.
-}
keepAlive : String -> Sub msg
keepAlive url =
  subscription (KeepAlive url)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
  case sub of
    Listen url tagger ->
      Listen url (tagger >> func)

    KeepAlive ->
      sub



-- MANAGER


type alias State msg =
  { sockets : SocketsDict
  , queues : QueuesDict
  , subs : SubsDict msg
  }


type alias SocketsDict =
  Dict.Dict String Connection


type alias QueuesDict =
  Dict.Dict String (List String)


type alias SubsDict msg =
  Dict.Dict String (List (String -> msg))


type Connection
  = Opening
  | Retry Int
  | Connected WS.WebSocket


init : Task Never State
init =
  Task.succeed (State Dict.empty Dict.empty Dict.empty)



-- HANDLE APP MESSAGES


onEffects
  : Platform.Router msg Msg
  -> List (MyCmd msg)
  -> List (MySub msg)
  -> State msg
  -> Task Never (State msg)
onEffects router cmds subs state =
  processCommands cmds state
    `Task.andThen` \newQueues ->

  let
    newSubs =
      toDictHelp subs Dict.empty

    names =
      Dict.keys newQueues ++ Dict.keys newSubs
        |> List.map (flip (,) ())
        |> Dict.fromList

    keepers =
      Dict.intersect state.sockets names

    closers =
      Dict.diff state.sockets names

    openers =
      Dict.diff names state.sockets
  in
    Task.sequence (List.map WS.close (Dict.values closers))
      `Task.andThen` \_ ->

    Task.sequence (List.map (attemptOpen self Nothing) (Dict.keys openers))
      `Task.andThen` \_ ->

    Task.succeed (Dict.union keepers (Dict.map (\_ _ -> Opening) openers))


processCommands : List (MyCmd msg) -> SocketsDict -> QueuesDict -> Task x QueuesDict
processCommands cmds sockets queues =
  case cmds of
    [] ->
      Task.succeed queues

    Send name msg :: rest ->
      case Dict.get name sockets of
        Nothing ->
          let
            newQueues =
              Dict.update name (add msg) queues
          in
            processCommands rest sockets newQueues

        Just socket ->
          WS.send socket msg
            `Task.andThen` \_ ->

          processCommands rest socket queues


toDictHelp : List (MySub msg) -> SubsDict msg -> SubsDict msg
toDictHelp subs dict =
  case subs of
    [] ->
      dict

    Listen name tagger :: rest ->
      toDictHelp rest (Dict.update name (add tagger) dict)

    KeepAlive name :: rest ->
      toDictHelp rest (Dict.update name (Just << Maybe.withDefault []) dict)


add : a -> Maybe (List a) -> Maybe (List a)
add value maybeList =
  case maybeList of
    Nothing ->
      Just []

    Just list ->
      Just (value :: list)



-- HANDLE SELF MESSAGES


type Msg
  = Receive String String
  | Die String
  | GoodOpen String WS.WebSocket
  | BadOpen String WS.BadOpen


onSelfMsg : Platform.Router msg Msg -> State msg -> Msg -> Task Never (State msg)
onSelfMsg app self state selfMsg =
  case selfMsg of
    Receive name str ->
      let
        taggers =
          Maybe.withDefault [] (Dict.get name state.subs)
      in
        Task.sequence (List.map (\tagger -> Process.send app (tagger str)))
          `Task.andThen` \_ ->

        Task.succeed state

    Die name ->
      case Dict.get name state.sockets of
        Nothing ->
          Task.succeed state

        Just _ ->
          attemptOpen self Nothing
            `Task.andThen` \_ ->

          Task.succeed
            { state
              | sockets =
                  Dict.insert name Opening state.sockets
            }

    GoodOpen name socket ->
      Task.succeed (updateSocket name (Connected socket) state)

    BadOpen name error ->
      case Dict.get name state.sockets of
        Nothing ->
          Task.succeed state

        Just Opening ->
          attemptOpen self (Just 1) name
            `Task.andThen` \_ ->

          Task.succeed (updateSocket name (Retry 1) state)

        Just (Retry n) ->
          attemptOpen self (Just (n + 1)) name
            `Task.andThen` \_ ->

          Task.succeed (updateSocket name (Retry (n + 1)) state)

        Just (Connected _) ->
          Task.succeed state


updateSocket : String -> Connection -> State msg -> State msg
updateSocket name connection state =
  { state | sockets = Dict.insert name connection state.sockets }


attemptOpen : Process a Msg -> Maybe Int -> String -> Task
attemptOpen self backoff name =
  after backoff
    `Task.andThen` \_ ->

  (WS.open name `Task.andThen` (Process.send self << GoodOpen))
    `Task.onError` (Process.send self << BadOpen)


after : Maybe Int -> Task x Time
after backoff =
  case backoff of
    Nothing ->
      Task.succeed 0

    Just delay ->
      Time.sleep (10 * 2 ^ delay)

