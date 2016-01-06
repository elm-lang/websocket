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


	return localRuntime.Native.WebSocket.values = {
	};
};
