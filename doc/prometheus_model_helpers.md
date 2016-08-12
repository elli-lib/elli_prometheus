

# Module prometheus_model_helpers #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#counter_metric-1">counter_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#counter_metric-2">counter_metric/2</a></td><td></td></tr><tr><td valign="top"><a href="#create_mf-5">create_mf/5</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metric-1">gauge_metric/1</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metric-2">gauge_metric/2</a></td><td></td></tr><tr><td valign="top"><a href="#gauge_metrics-1">gauge_metrics/1</a></td><td></td></tr><tr><td valign="top"><a href="#histogram_metric-4">histogram_metric/4</a></td><td></td></tr><tr><td valign="top"><a href="#label_pairs-1">label_pairs/1</a></td><td></td></tr><tr><td valign="top"><a href="#summary_metric-3">summary_metric/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="counter_metric-1"></a>

### counter_metric/1 ###

`counter_metric(Value) -> any()`

<a name="counter_metric-2"></a>

### counter_metric/2 ###

`counter_metric(Labels, Value) -> any()`

<a name="create_mf-5"></a>

### create_mf/5 ###

`create_mf(Name, Help, Type, Collector, CollectorData) -> any()`

<a name="gauge_metric-1"></a>

### gauge_metric/1 ###

`gauge_metric(Value) -> any()`

<a name="gauge_metric-2"></a>

### gauge_metric/2 ###

`gauge_metric(Labels, Value) -> any()`

<a name="gauge_metrics-1"></a>

### gauge_metrics/1 ###

`gauge_metrics(Metrics) -> any()`

<a name="histogram_metric-4"></a>

### histogram_metric/4 ###

`histogram_metric(Labels, Buckets, Count, Sum) -> any()`

<a name="label_pairs-1"></a>

### label_pairs/1 ###

`label_pairs(Labels) -> any()`

<a name="summary_metric-3"></a>

### summary_metric/3 ###

`summary_metric(Labels, Count, Sum) -> any()`

