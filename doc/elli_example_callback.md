

# Module elli_example_callback #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli example callback.

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="description"></a>

## Description ##
Your callback needs to implement two functions, handle/2 and
handle_event/3. For every request, Elli will call your handle
function with the request. When an event happens, like Elli
completed a request, there was a parsing error or your handler
threw an error, handle_event/3 is called.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Ref) -> any()`

<a name="handle-2"></a>

### handle/2 ###

`handle(Req, Args) -> any()`

<a name="handle_event-3"></a>

### handle_event/3 ###

`handle_event(X1, X2, X3) -> any()`

