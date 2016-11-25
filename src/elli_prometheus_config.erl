-module(elli_prometheus_config).
-author("Ilya Khaprov").

%% API.
-export([path/0,
         format/0,
         allowed_formats/0,
         duration_buckets/0,
         labels/0]).

%% Macros.
-define(DEFAULT_PATH, <<"/metrics">>).
-define(DEFAULT_FORMAT, auto).
-define(DEFAULT_DURATION_BUCKETS, [10, 100, 1000, 10000, 100000, 300000, 500000,
                                   750000, 1000000, 1500000, 2000000, 3000000]).
-define(DEFAULT_LABELS, [method, handler, status_class]).

-define(DEFAULT_CONFIG, [{path, ?DEFAULT_PATH},
                         {format, ?DEFAULT_FORMAT},
                         {duration_buckets, ?DEFAULT_DURATION_BUCKETS},
                         {labels, ?DEFAULT_LABELS}]).

%%%===================================================================
%%% API
%%%===================================================================

path() -> get_value(path, ?DEFAULT_PATH).

format() -> get_value(format, ?DEFAULT_FORMAT).

allowed_formats() ->
  [{prometheus_text_format:content_type(), prometheus_text_format},
   {prometheus_protobuf_format:content_type(), prometheus_protobuf_format}].

duration_buckets() -> get_value(duration_buckets, ?DEFAULT_DURATION_BUCKETS).

labels() -> get_value(labels, ?DEFAULT_LABELS).

%%%===================================================================
%%% Private functions
%%%===================================================================

get_value(Key, Default) -> proplists:get_value(Key, config(), Default).

config() -> application:get_env(prometheus, elli_exporter, ?DEFAULT_CONFIG).
