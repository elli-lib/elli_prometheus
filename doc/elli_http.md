

# Module elli_http #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli HTTP request implementation.

<a name="description"></a>

## Description ##
An elli_http process blocks in elli_tcp:accept/2 until a client
connects. It then handles requests on that connection until it's
closed either by the client timing out or explicitly by the user.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-4">accept/4</a></td><td> Accept on the socket until a client connects.</td></tr><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_request-4">handle_request/4</a></td><td> Handle a HTTP request that will possibly come on the
socket.</td></tr><tr><td valign="top"><a href="#keepalive_loop-3">keepalive_loop/3</a></td><td> Handle multiple requests on the same connection, ie.</td></tr><tr><td valign="top"><a href="#keepalive_loop-5">keepalive_loop/5</a></td><td></td></tr><tr><td valign="top"><a href="#mk_req-7">mk_req/7</a></td><td></td></tr><tr><td valign="top"><a href="#parse_path-1">parse_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_response-4">send_response/4</a></td><td> Generates a HTTP response and sends it to the client.</td></tr><tr><td valign="top"><a href="#split_args-1">split_args/1</a></td><td> Splits the url arguments into a proplist.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-4"></a>

### accept/4 ###

<pre><code>
accept(Server::pid(), ListenSocket::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>, Options::<a href="proplists.md#type-proplist">proplists:proplist()</a>, Callback::<a href="#type-callback">callback()</a>) -&gt; ok
</code></pre>
<br />

Accept on the socket until a client connects. Handles the
request, then loops if we're using keep alive or chunked
transfer. If accept doesn't give us a socket within a configurable
timeout, we loop to allow code upgrades of this module.

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Socket) -> any()`

<a name="handle_request-4"></a>

### handle_request/4 ###

<pre><code>
handle_request(S::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>, PrevB::binary(), Opts::<a href="proplists.md#type-proplist">proplists:proplist()</a>, Callback::<a href="#type-callback">callback()</a>) -&gt; {keep_alive | close, binary()}
</code></pre>
<br />

Handle a HTTP request that will possibly come on the
socket. Returns the appropriate connection token and any buffer
containing (parts of) the next request.

<a name="keepalive_loop-3"></a>

### keepalive_loop/3 ###

`keepalive_loop(Socket, Options, Callback) -> any()`

Handle multiple requests on the same connection, ie. "keep
alive".

<a name="keepalive_loop-5"></a>

### keepalive_loop/5 ###

`keepalive_loop(Socket, NumRequests, Buffer, Options, Callback) -> any()`

<a name="mk_req-7"></a>

### mk_req/7 ###

<pre><code>
mk_req(Method::<a href="#type-http_method">http_method()</a>, RawPath::{PathType::atom(), RawPath::binary()}, RequestHeaders::<a href="#type-headers">headers()</a>, RequestBody::<a href="#type-body">body()</a>, V::<a href="#type-version">version()</a>, Socket::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a> | undefined, Callback::<a href="#type-callback">callback()</a>) -&gt; #req{}
</code></pre>
<br />

<a name="parse_path-1"></a>

### parse_path/1 ###

`parse_path(X1) -> any()`

<a name="send_response-4"></a>

### send_response/4 ###

`send_response(Req, Code, Headers, UserBody) -> any()`

Generates a HTTP response and sends it to the client

<a name="split_args-1"></a>

### split_args/1 ###

<pre><code>
split_args(Qs::binary()) -&gt; [{binary(), binary() | true}]
</code></pre>
<br />

Splits the url arguments into a proplist. Lifted from
cowboy_http:x_www_form_urlencoded/2

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Server::pid(), ListenSocket::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>, Options::<a href="proplists.md#type-proplist">proplists:proplist()</a>, Callback::<a href="#type-callback">callback()</a>) -&gt; pid()
</code></pre>
<br />

