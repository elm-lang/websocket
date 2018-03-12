/*

import Elm.Kernel.Scheduler exposing (binding, succeed, fail, rawSpawn)
import Maybe exposing (Just, Nothing)
import WebSocket.LowLevel as WS exposing (BadSecurity, BadArgs, NotOpen, BadString, BadReason, BadCode)

*/


var _WebSocket_open = F2(function(url, settings)
{
	return __Scheduler_binding(function(callback)
	{
		try
		{
			var socket = new WebSocket(url);
		}
		catch(err)
		{
			return callback(__Scheduler_fail(
				err.name === 'SecurityError' ? __WS_BadSecurity : __WS_BadArgs
			));
		}

		socket.addEventListener("open", function(event) {
			callback(__Scheduler_succeed(socket));
		});

		socket.addEventListener("message", function(event) {
			__Scheduler_rawSpawn(A2(settings.onMessage, socket, event.data));
		});

		socket.addEventListener("close", function(event) {
			__Scheduler_rawSpawn(settings.onClose({
				__$code: event.code,
				__$reason: event.reason,
				__$wasClean: event.wasClean
			}));
		});

		return function()
		{
			if (socket && socket.close)
			{
				socket.close();
			}
		};
	});
});


var _WebSocket_send = F2(function(socket, string)
{
	return __Scheduler_binding(function(callback)
	{
		var result =
			socket.readyState === WebSocket.OPEN
				? __Maybe_Nothing
				: __Maybe_Just(__WS_NotOpen);

		try
		{
			socket.send(string);
		}
		catch(err)
		{
			result = __Maybe_Just(__WS_BadString);
		}

		callback(__Scheduler_succeed(result));
	});
});


var _WebSocket_close = F3(function(code, reason, socket)
{
	return __Scheduler_binding(function(callback) {
		try
		{
			socket.close(code, reason);
		}
		catch(err)
		{
			return callback(__Scheduler_fail(__Maybe_Just({
				err.name === 'SyntaxError' ? __WS_BadReason : __WS_BadCode
			})));
		}
		callback(__Scheduler_succeed(__Maybe_Nothing));
	});
});


function _WebSocket_bytesQueued(socket)
{
	return __Scheduler_binding(function(callback) {
		callback(__Scheduler_succeed(socket.bufferedAmount));
	});
}
