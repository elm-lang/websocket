module WebSocket
    ( WebSocket
    , Message
    , open, Settings
    , send, close, closeWith
    , queueSize
    )
    where
{-|

# WebSockets and Messages
@docs WebSocket, Message

# Using WebSockets
@docs open, Settings, send, close, closeWith, queueSize

-}

import Native.WebSocket


type WebSocket = WebSocket


{-|

url - The URL to which to connect; this should be the URL to which the WebSocket server will respond.
protocols - Either a single protocol string or an array of protocol strings. These strings are used to indicate sub-protocols, so that a single server can implement multiple WebSocket sub-protocols (for example, you might want one server to be able to handle different types of interactions depending on the specified protocol). If you don't specify a protocol string, an empty string is assumed.

SECURITY_ERR - The port to which the connection is being attempted is being blocked
-}
open : String -> Settings -> Task SecurityError WebSocket
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
  { onMessage : WebSocket -> Message -> Task Never ()
  , onClose : { code : Int, reason : String, wasClean : Bool } -> Task Never ()
  }


{-| The WebSockets protocol in browsers allows you to send strings, blobs, and
binary arrays. It is quite rare to use things besides strings, but it can
happen! So in your `onMessage` handler, you need to account for all of these
cases.

**Note:** Right now there is not a native representation of blobs or binary
arrays, so you just get a `Json.Encode.Value` that can be sent through ports
but nothing else. This will improve as soon as blobs and binary arrays become
part of the core `elm-lang` platform libraries.
-}
type Message
    = String String
    | Blob Json.Value
    | Binary Json.Value


{-| Close a `WebSocket`. If the connection is already closed, it does nothing.
-}
close : WebSocket -> Task x ()
close ws =
  closeWith 1000 "" ws


{-| Closes the `WebSocket`. If the connection is already closed, it does nothing.

In addition to providing the `WebSocket` you want to close, you must provide:

  1. A status code explaining why the connection is being closed. The default
  value is 1000, indicating indicates a normal "transaction complete" closure.
  There are a ton of different status codes though. See them all
  [here](https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent).

  2. A human-readable string explaining why the connection is closing. This
  string must be no longer than 123 bytes of UTF-8 text (not characters).

-}
closeWith : Int -> String -> WebSocket -> Task BadClose ()
closeWith =
  Native.WebSocket.close


{-|
  INVALID_ACCESS_ERR - An invalid code was specified.
  SYNTAX_ERR - The reason string is too long or contains unpaired surrogates.
-}
type BadClose
    = BadCode
    | BadReason


{-| Transmits data to the server over the WebSocket connection.

data - A text string to send to the server.

-}
send : WebSocket -> String -> Task x ()
send =
  Native.WebSocket.send


{-| INVALID_STATE_ERR - The connection is not currently OPEN.
SYNTAX_ERR - The data is a string that has unpaired surrogates.
-}
type BadSend
    = NotOpen
    | BadMessage


{-| The number of bytes of data queued by `send` but not yet transmitted to the
network.
-}
queueSize : WebSocket -> Task x Int
queueSize =
  Native.WebSocket.queueSize

