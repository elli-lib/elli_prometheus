

# Module elli_request #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_send_chunk-2">async_send_chunk/2</a></td><td> Sends a chunk asynchronously.</td></tr><tr><td valign="top"><a href="#body-1">body/1</a></td><td></td></tr><tr><td valign="top"><a href="#body_qs-1">body_qs/1</a></td><td>Parses application/x-www-form-urlencoded body into a proplist.</td></tr><tr><td valign="top"><a href="#chunk_ref-1">chunk_ref/1</a></td><td> Returns a reference that can be used to send chunks to the
client.</td></tr><tr><td valign="top"><a href="#close_chunk-1">close_chunk/1</a></td><td> Explicitly close the chunked connection.</td></tr><tr><td valign="top"><a href="#get_arg-2">get_arg/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_arg-3">get_arg/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_arg_decoded-2">get_arg_decoded/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_arg_decoded-3">get_arg_decoded/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_args-1">get_args/1</a></td><td>Returns a proplist of keys and values of the original query
string.</td></tr><tr><td valign="top"><a href="#get_args_decoded-1">get_args_decoded/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_header-2">get_header/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_header-3">get_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_range-1">get_range/1</a></td><td> Parses the Range header from the request.</td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_request-1">is_request/1</a></td><td></td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td></td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td> Returns path split into binary parts.</td></tr><tr><td valign="top"><a href="#peer-1">peer/1</a></td><td></td></tr><tr><td valign="top"><a href="#post_arg-2">post_arg/2</a></td><td></td></tr><tr><td valign="top"><a href="#post_arg-3">post_arg/3</a></td><td></td></tr><tr><td valign="top"><a href="#post_arg_decoded-2">post_arg_decoded/2</a></td><td></td></tr><tr><td valign="top"><a href="#post_arg_decoded-3">post_arg_decoded/3</a></td><td></td></tr><tr><td valign="top"><a href="#post_args-1">post_args/1</a></td><td></td></tr><tr><td valign="top"><a href="#post_args_decoded-1">post_args_decoded/1</a></td><td></td></tr><tr><td valign="top"><a href="#query_str-1">query_str/1</a></td><td>Calculates the query string associated with the given Request
as a binary.</td></tr><tr><td valign="top"><a href="#raw_path-1">raw_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_chunk-2">send_chunk/2</a></td><td> Sends a chunk synchronously, if the refrenced process is dead
returns early with {error, closed} instead of timing out.</td></tr><tr><td valign="top"><a href="#to_proplist-1">to_proplist/1</a></td><td> Serializes the request record to a proplist.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_send_chunk-2"></a>

### async_send_chunk/2 ###

`async_send_chunk(Ref, Data) -> any()`

Sends a chunk asynchronously

<a name="body-1"></a>

### body/1 ###

`body(Req) -> any()`

<a name="body_qs-1"></a>

### body_qs/1 ###

`body_qs(Req) -> any()`

Parses application/x-www-form-urlencoded body into a proplist

<a name="chunk_ref-1"></a>

### chunk_ref/1 ###

`chunk_ref(Req) -> any()`

Returns a reference that can be used to send chunks to the
client. If the protocol does not support it, returns {error,
not_supported}.

<a name="close_chunk-1"></a>

### close_chunk/1 ###

`close_chunk(Ref) -> any()`

Explicitly close the chunked connection. Returns {error,
closed} if the client already closed the connection.

<a name="get_arg-2"></a>

### get_arg/2 ###

`get_arg(Key, Req) -> any()`

<a name="get_arg-3"></a>

### get_arg/3 ###

`get_arg(Key, Req, Default) -> any()`

<a name="get_arg_decoded-2"></a>

### get_arg_decoded/2 ###

`get_arg_decoded(Key, Req) -> any()`

<a name="get_arg_decoded-3"></a>

### get_arg_decoded/3 ###

`get_arg_decoded(Key, Req, Default) -> any()`

<a name="get_args-1"></a>

### get_args/1 ###

<pre><code>
get_args(Req::#req{}) -&gt; QueryArgs::<a href="proplists.md#type-proplist">proplists:proplist()</a>
</code></pre>
<br />

Returns a proplist of keys and values of the original query
string.  Both keys and values in the returned proplists will
be binaries or the atom `true` in case no value was supplied
for the query value.

<a name="get_args_decoded-1"></a>

### get_args_decoded/1 ###

`get_args_decoded(Req) -> any()`

<a name="get_header-2"></a>

### get_header/2 ###

`get_header(Key, Req) -> any()`

<a name="get_header-3"></a>

### get_header/3 ###

`get_header(Key, Req, Default) -> any()`

<a name="get_range-1"></a>

### get_range/1 ###

<pre><code>
get_range(Req::#req{}) -&gt; [<a href="#type-http_range">http_range()</a>] | parse_error
</code></pre>
<br />

Parses the Range header from the request.
The result is either a byte_range_set() or the atom `parse_error`.
Use elli_util:normalize_range/2 to get a validated, normalized range.

<a name="headers-1"></a>

### headers/1 ###

`headers(Req) -> any()`

<a name="is_request-1"></a>

### is_request/1 ###

`is_request(Req) -> any()`

<a name="method-1"></a>

### method/1 ###

`method(Req) -> any()`

<a name="path-1"></a>

### path/1 ###

`path(Req) -> any()`

Returns path split into binary parts.

<a name="peer-1"></a>

### peer/1 ###

`peer(Req) -> any()`

<a name="post_arg-2"></a>

### post_arg/2 ###

`post_arg(Key, Req) -> any()`

<a name="post_arg-3"></a>

### post_arg/3 ###

`post_arg(Key, Req, Default) -> any()`

<a name="post_arg_decoded-2"></a>

### post_arg_decoded/2 ###

`post_arg_decoded(Key, Req) -> any()`

<a name="post_arg_decoded-3"></a>

### post_arg_decoded/3 ###

`post_arg_decoded(Key, Req, Default) -> any()`

<a name="post_args-1"></a>

### post_args/1 ###

`post_args(Req) -> any()`

<a name="post_args_decoded-1"></a>

### post_args_decoded/1 ###

`post_args_decoded(Req) -> any()`

<a name="query_str-1"></a>

### query_str/1 ###

<pre><code>
query_str(Req::#req{}) -&gt; QueryStr::binary()
</code></pre>
<br />

Calculates the query string associated with the given Request
as a binary.

<a name="raw_path-1"></a>

### raw_path/1 ###

`raw_path(Req) -> any()`

<a name="send_chunk-2"></a>

### send_chunk/2 ###

`send_chunk(Ref, Data) -> any()`

Sends a chunk synchronously, if the refrenced process is dead
returns early with {error, closed} instead of timing out.

<a name="to_proplist-1"></a>

### to_proplist/1 ###

`to_proplist(Req) -> any()`

Serializes the request record to a proplist. Useful for
logging

