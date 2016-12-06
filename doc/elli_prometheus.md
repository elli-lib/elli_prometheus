

# Module elli_prometheus #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli middleware for collecting stats via Prometheus.

Copyright (c) 2016 elli-lib team

__Version:__ 0.1.0

__Behaviours:__ [`elli_handler`](https://github.com/elli-lib/elli/blob/develop/doc/elli_handler.md).

__Authors:__ Eric Bailey, Ilya Khaprov.

__References__* [Prometheus](https://prometheus.io)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>Handle requests to <code>/metrics</code> and ignore all others.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle-2"></a>

### handle/2 ###

`handle(Req, Config) -> any()`

Handle requests to `/metrics` and ignore all others.

__<font color="red">To do</font>__<br />* <font color="red"> Describe format.</font>
* <font color="red"> Add links to Prometheus and Prometheus.erl docs.</font>

<a name="handle_event-3"></a>

### handle_event/3 ###

`handle_event(Event, Args, Config) -> any()`

