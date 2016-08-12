

# Module elli_test #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Helper for calling your Elli callback in unit tests.

__Authors:__ Andreas Hasselberg ([`andreas.hasselberg@gmail.com`](mailto:andreas.hasselberg@gmail.com)).

<a name="description"></a>

## Description ##
Only the
callback specified is actually run. Ellis response handling is
not used, so the headers will for example not include a content
length and the return format is not standardized.  The unit tests
below tests elli_example_callback.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-5">call/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-5"></a>

### call/5 ###

<pre><code>
call(Method::<a href="#type-http_method">http_method()</a>, Path::binary(), Headers::<a href="#type-headers">headers()</a>, Body::<a href="#type-body">body()</a>, Opts::<a href="proplists.md#type-proplist">proplists:proplist()</a>) -&gt; #req{}
</code></pre>
<br />

