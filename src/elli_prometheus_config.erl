-module(elli_prometheus_config).

-export([path/0,
         format/0,
         duration_buckets/0,
         labels/0]).

-define(DEFAULT_PATH, <<"/metrics">>).
-define(DEFAULT_FORMAT, prometheus_text_format).
-define(DEFAULT_DURATION_BUCKETS, [10,100,1000,10000,100000,300000,500000,
                                   750000,1000000,1500000,2000000,3000000]).
-define(DEFAULT_LABELS, [method,handler,status_class]).

-define(DEFAULT_CONFIG, [{path,?DEFAULT_PATH},
                         {format,?DEFAULT_FORMAT},
                         {duration_buckets,?DEFAULT_DURATION_BUCKETS},
                         {labels,?DEFAULT_LABELS}]).

config() ->
  application:get_env(prometheus, elli_exporter, ?DEFAULT_CONFIG).

path() ->
  Config = config(),
  proplists:get_value(path, Config, ?DEFAULT_PATH).

format() ->
  Config = config(),
  proplists:get_value(format, Config, ?DEFAULT_FORMAT).

duration_buckets() ->
  Config = config(),
  proplists:get_value(duration_buckets, Config, ?DEFAULT_DURATION_BUCKETS).

labels() ->
  Config = config(),
  proplists:get_value(labels, Config, ?DEFAULT_LABELS).
