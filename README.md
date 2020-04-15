# WebSockets

The recommended way to use WebSockets with Elm for now is through ports. You can see a minimal example in the [`js-integration-examples`](https://github.com/elm-community/js-integration-examples/) repo:

![echo demo](https://raw.githubusercontent.com/elm-community/js-integration-examples/master/websockets/demo.gif)

You can play with a live version of this example [here](https://ellie-app.com/8yYgw7y7sM2a1).


<br>

<br>

### History

We had a bare bones version of WebSockets in within Elm in versions 0.17 and 0.18, part of the introduction of subscriptions to Elm. But users found that the API was not able to cover a lot of situations they faced in practice. How can this work with Elixir Pheonix? Firebase? How can I use a different backoff strategy for reconnecting? How can I hear about when the connection goes down or comes back? How about sub-protocols?

In trying to expand the API to cover all the cases people were facing in practice, I came to think that it may not be possible with the current subscriptions infrastructure. (My feeling is that effect managers may not be a great fit for web sockets because they do not have great mechanisms for uniquely identifying resources. Do we have one connections or two? How do we tell the difference? If it requires function pointer equality, how can we make that reliable when an anonymous function is used?) I did not understand this problem as well in [2016](https://elm-lang.org/news/farewell-to-frp), and I think it manifested most clearly with web sockets.

So facing the prospect of either (1) having an API that many eventually had to leave behind for ports or (2) recommending that people go with ports from the start, we figured that (2) was probably the best for someone new coming to Elm. That way they would hook up to their preferred web socket manager without the intermediate step of learning a promising but incomplete API.
