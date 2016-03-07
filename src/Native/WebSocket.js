Elm.Native.WebSocket = {};
Elm.Native.WebSocket.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.WebSocket = localRuntime.Native.WebSocket || {};
	if (localRuntime.Native.WebSocket.values)
	{
		return localRuntime.Native.WebSocket.values;
	}

	var Maybe = Elm.Maybe.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function open(url, settings)
	{
		return Task.asyncFunction(function(callback) {
			try
			{
				var socket = new WebSocket(url);
			}
			catch(err)
			{
				return callback(Task.fail({
					ctor: err.name == "SecurityError"  ? 'BadSecurity' : 'BadArgs',
					_0: err.message
				}));
			}

			socket.addEventListener("message", function(evt) {
				Task.perform(settings.onMessage(socket)(evt.data));
			});

			socket.addEventListener("close", function(evt) {
				Task.perform(settings.onClose({
					code: evt.code,
					reason: evt.reason,
					wasClean: evt.wasClean
				}));
			});

			socket.addEventListener("open", function(evt) {
				callback(Task.succeed(socket));
			});
		});
	}

	function send(socket, string)
	{
		return Task.asyncFunction(function(callback) {
			var result =
				socket.readyState === WebSocket.OPEN
					? Maybe.Nothing
					: Maybe.Just({ ctor: 'NotOpen' });

			try
			{
				socket.send(string);
			}
			catch(err)
			{
				result = Maybe.Just({ ctor: 'BadString' });
			}

			callback(Task.succeed(result));
		});
	}

	function close(code, reason, socket)
	{
		return Task.asyncFunction(function(callback) {
			try
			{
				socket.close(code, reason);
			}
			catch(err)
			{
				return callback(Task.fail({
					ctor: err.name == "SyntaxError" ? 'BadReason' : 'BadCode',
					_0: err.message
				}));
			}
			callback(Task.succeed(Utils.Tuple0));
		});
	}

	function bytesQueued(socket)
	{
		return Task.asyncFunction(function(callback) {
			callback(Task.succeed(socket.bufferedAmount));
		});
	}

	return localRuntime.Native.WebSocket.values = {
		open: F2(open),
		send: F2(send),
		close: F3(close),
		bytesQueued: bytesQueued
	};
};
