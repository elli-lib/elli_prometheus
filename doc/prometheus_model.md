

# Module prometheus_model #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-Bucket">'Bucket'()</a> ###


<pre><code>
'Bucket'() = #'Bucket'{}
</code></pre>




### <a name="type-Counter">'Counter'()</a> ###


<pre><code>
'Counter'() = #'Counter'{}
</code></pre>




### <a name="type-Gauge">'Gauge'()</a> ###


<pre><code>
'Gauge'() = #'Gauge'{}
</code></pre>




### <a name="type-Histogram">'Histogram'()</a> ###


<pre><code>
'Histogram'() = #'Histogram'{}
</code></pre>




### <a name="type-LabelPair">'LabelPair'()</a> ###


<pre><code>
'LabelPair'() = #'LabelPair'{}
</code></pre>




### <a name="type-Metric">'Metric'()</a> ###


<pre><code>
'Metric'() = #'Metric'{}
</code></pre>




### <a name="type-MetricFamily">'MetricFamily'()</a> ###


<pre><code>
'MetricFamily'() = #'MetricFamily'{}
</code></pre>




### <a name="type-MetricType">'MetricType'()</a> ###


<pre><code>
'MetricType'() = 'COUNTER' | 'GAUGE' | 'SUMMARY' | 'UNTYPED' | 'HISTOGRAM'
</code></pre>




### <a name="type-Quantile">'Quantile'()</a> ###


<pre><code>
'Quantile'() = #'Quantile'{}
</code></pre>




### <a name="type-Summary">'Summary'()</a> ###


<pre><code>
'Summary'() = #'Summary'{}
</code></pre>




### <a name="type-Untyped">'Untyped'()</a> ###


<pre><code>
'Untyped'() = #'Untyped'{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_msg-2">decode_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode_msg-3">decode_msg/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_msg-1">encode_msg/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_msg-2">encode_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#enum_symbol_by_value-2">enum_symbol_by_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#enum_symbol_by_value_MetricType-1">enum_symbol_by_value_MetricType/1</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol-2">enum_value_by_symbol/2</a></td><td></td></tr><tr><td valign="top"><a href="#enum_value_by_symbol_MetricType-1">enum_value_by_symbol_MetricType/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_enum_def-1">fetch_enum_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_msg_def-1">fetch_msg_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_rpc_def-2">fetch_rpc_def/2</a></td><td></td></tr><tr><td valign="top"><a href="#find_enum_def-1">find_enum_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_msg_def-1">find_msg_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#find_rpc_def-2">find_rpc_def/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_enum_names-0">get_enum_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg_defs-0">get_msg_defs/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_msg_names-0">get_msg_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_package_name-0">get_package_name/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_rpc_names-1">get_rpc_names/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_service_def-1">get_service_def/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_service_names-0">get_service_names/0</a></td><td></td></tr><tr><td valign="top"><a href="#gpb_version_as_list-0">gpb_version_as_list/0</a></td><td></td></tr><tr><td valign="top"><a href="#gpb_version_as_string-0">gpb_version_as_string/0</a></td><td></td></tr><tr><td valign="top"><a href="#merge_msgs-2">merge_msgs/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge_msgs-3">merge_msgs/3</a></td><td></td></tr><tr><td valign="top"><a href="#verify_msg-1">verify_msg/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify_msg-2">verify_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-2"></a>

### decode/2 ###

`decode(MsgName, Bin) -> any()`

<a name="decode_msg-2"></a>

### decode_msg/2 ###

`decode_msg(Bin, MsgName) -> any()`

<a name="decode_msg-3"></a>

### decode_msg/3 ###

`decode_msg(Bin, MsgName, Opts) -> any()`

<a name="encode-1"></a>

### encode/1 ###

`encode(Msg) -> any()`

<a name="encode_msg-1"></a>

### encode_msg/1 ###

<pre><code>
encode_msg(Msg::term()) -&gt; binary()
</code></pre>
<br />

<a name="encode_msg-2"></a>

### encode_msg/2 ###

<pre><code>
encode_msg(Msg::term(), Opts::[]) -&gt; binary()
</code></pre>
<br />

<a name="enum_symbol_by_value-2"></a>

### enum_symbol_by_value/2 ###

`enum_symbol_by_value(X1, Value) -> any()`

<a name="enum_symbol_by_value_MetricType-1"></a>

### enum_symbol_by_value_MetricType/1 ###

`enum_symbol_by_value_MetricType(X1) -> any()`

<a name="enum_value_by_symbol-2"></a>

### enum_value_by_symbol/2 ###

`enum_value_by_symbol(X1, Sym) -> any()`

<a name="enum_value_by_symbol_MetricType-1"></a>

### enum_value_by_symbol_MetricType/1 ###

`enum_value_by_symbol_MetricType(X1) -> any()`

<a name="fetch_enum_def-1"></a>

### fetch_enum_def/1 ###

`fetch_enum_def(EnumName) -> any()`

<a name="fetch_msg_def-1"></a>

### fetch_msg_def/1 ###

`fetch_msg_def(MsgName) -> any()`

<a name="fetch_rpc_def-2"></a>

### fetch_rpc_def/2 ###

<pre><code>
fetch_rpc_def(ServiceName::term(), RpcName::term()) -&gt; no_return()
</code></pre>
<br />

<a name="find_enum_def-1"></a>

### find_enum_def/1 ###

`find_enum_def(X1) -> any()`

<a name="find_msg_def-1"></a>

### find_msg_def/1 ###

`find_msg_def(X1) -> any()`

<a name="find_rpc_def-2"></a>

### find_rpc_def/2 ###

`find_rpc_def(X1, X2) -> any()`

<a name="get_enum_names-0"></a>

### get_enum_names/0 ###

`get_enum_names() -> any()`

<a name="get_msg_defs-0"></a>

### get_msg_defs/0 ###

`get_msg_defs() -> any()`

<a name="get_msg_names-0"></a>

### get_msg_names/0 ###

`get_msg_names() -> any()`

<a name="get_package_name-0"></a>

### get_package_name/0 ###

`get_package_name() -> any()`

<a name="get_rpc_names-1"></a>

### get_rpc_names/1 ###

`get_rpc_names(X1) -> any()`

<a name="get_service_def-1"></a>

### get_service_def/1 ###

`get_service_def(X1) -> any()`

<a name="get_service_names-0"></a>

### get_service_names/0 ###

`get_service_names() -> any()`

<a name="gpb_version_as_list-0"></a>

### gpb_version_as_list/0 ###

`gpb_version_as_list() -> any()`

<a name="gpb_version_as_string-0"></a>

### gpb_version_as_string/0 ###

`gpb_version_as_string() -> any()`

<a name="merge_msgs-2"></a>

### merge_msgs/2 ###

`merge_msgs(Prev, New) -> any()`

<a name="merge_msgs-3"></a>

### merge_msgs/3 ###

`merge_msgs(Prev, New, Opts) -> any()`

<a name="verify_msg-1"></a>

### verify_msg/1 ###

`verify_msg(Msg) -> any()`

<a name="verify_msg-2"></a>

### verify_msg/2 ###

`verify_msg(Msg, Opts) -> any()`

