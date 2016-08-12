

# Module elli_prometheus #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli middleware for collecting stats via Prometheus.

Copyright (c) 2016 Eric Bailey

__Version:__ 0.0.1

__Behaviours:__ [`elli_handler`](elli_handler.md).

__Authors:__ Eric Bailey.

__References__
* [Prometheus](https://prometheus.io)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>Handle requests to <code>/metrics</code> and ignore all others.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td>On <code>elli_startup</code>, register two metrics, a counter <code>http_requests_total</code>
and a histogram <code>http_request_duration_microseconds</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle-2"></a>

### handle/2 ###

`handle(Req, Config) -> any()`

Handle requests to `/metrics` and ignore all others.

__<font color="red">To do</font>__<br />
* <font color="red"> Describe format.</font>
* <font color="red"> Add links to Prometheus and Prometheus.erl docs.</font>
* <font color="red"> Make path configurable.</font>

<a name="handle_event-3"></a>

### handle_event/3 ###

`handle_event(Event, Args, Config) -> any()`

On `elli_startup`, register two metrics, a counter `http_requests_total`
and a histogram `http_request_duration_microseconds`. Then, on
`request_complete`, [increment](prometheus_counter.md#inc-2)
`http_requests_total` and [observe](prometheus_histogram.md#observe-3)
`http_request_duration_microseconds`. Ignore all other events.
