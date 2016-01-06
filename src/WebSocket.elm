module WebSocket
    ( WebSocket
    , Message
    , open, Settings
    , send, close, closeWith
    , bytesQueued
    )
    where
{-|

# WebSockets and Messages
@docs WebSocket, Message

# Using WebSockets
@docs open, Settings, send, close, closeWith, bytesQueued

-}

import Native.WebSocket


type WebSocket = WebSocket


{-|
-}
open : String -> Settings -> Task BadOpen WebSocket
open =
  Native.WebSocket.open


{-| The settings describe how a `WebSocket` works as long as it is still open.

The `onMessage` function gives you access to (1) the `WebSocket` itself so you
can use functions like `send` and `close` and (2) the `Message` from the server
so you can decide what to do next.

The `onClose` function tells you everything about why the `WebSocket` is
closing. There are a ton of codes with standardized meanings, so learn more
about them [here](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent).

You will typically want to set up a channel before opening a WebSocket. That
way the `onMessage` and `onClose` can communicate with the other parts of your
program. **Ideally this is handled by the effect library you are using though.
Most people should not be working with this stuff directly.**
-}
type alias Settings =
  { onMessage : WebSocket -> String -> Task Never ()
  , onClose : { code : Int, reason : String, wasClean : Bool } -> Task Never ()
  }


type BadOpen
    = BadSecurity
    | BadArgs


{-| Close a `WebSocket`. If the connection is already closed, it does nothing.
-}
close : WebSocket -> Task x ()
close socket =
  Task.map
    (always ())
    (closeWith 1000 "" socket)


{-| Closes the `WebSocket`. If the connection is already closed, it does nothing.

In addition to providing the `WebSocket` you want to close, you must provide:

  1. A status code explaining why the connection is being closed. The default
  value is 1000, indicating indicates a normal "transaction complete" closure.
  There are a ton of different status codes though. See them all
  [here](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent).

  2. A human-readable string explaining why the connection is closing. This
  string must be no longer than 123 bytes of UTF-8 text (not characters).

-}
closeWith : Int -> String -> WebSocket -> Task x (Maybe BadClose)
closeWith =
  Native.WebSocket.close


{-| It is possible to provide invalid codes or reasons for closing a
connection. The connection will still be closed, but the `closeWith` function
will give you `BadCode` if an invalid code was specified or `BadReason` if your
reason is too long or contains unpaired surrogates.
-}
type BadClose
    = BadCode
    | BadReason


{-| Send a string over the `WebSocket` to the server. If there is any problem
with the send, you will get some data about it as the result of running this
task.
-}
send : WebSocket -> String -> Task x (Maybe BadSend)
send =
  Native.WebSocket.send


{-| There are a few ways a send can go wrong. The send function will ultimately
give you a `NotOpen` if the connection is no longer open or a `BadString` if
the string has unpaired surrogates (badly formatted UTF-16).
-}
type BadSend
    = NotOpen
    | BadString


{-| The number of bytes of data queued by `send` but not yet transmitted to the
network. If you have been sending data to a closed connection, it will just
pile up on the queue endlessly.
-}
bytesQueued : WebSocket -> Task x Int
bytesQueued =
  Native.WebSocket.bytesQueued

