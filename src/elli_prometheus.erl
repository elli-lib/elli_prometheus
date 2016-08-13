%% @doc Elli middleware for collecting stats via Prometheus.
%% @author Eric Bailey
%% @version 0.0.1
%% @reference <a href="https://prometheus.io">Prometheus</a>
%% @copyright 2016 Eric Bailey
-module(elli_prometheus).
-author("Eric Bailey").

-behaviour(elli_handler).

%% elli_handler callbacks
-export([handle/2,handle_event/3]).

%% Macros.
-define(LABELS,   [method,handler,status_code]).
-define(TOTAL,    http_requests_total).
-define(DURATION, http_request_duration_microseconds).

%%%===================================================================
%%% elli_handler callbacks
%%%===================================================================

%% @doc Handle requests to `/metrics' and ignore all others.
%% TODO: Describe format.
%% TODO: Add links to Prometheus and Prometheus.erl docs.
%% TODO: Make path configurable.
handle(Req, _Config) ->
  case {elli_request:method(Req),elli_request:raw_path(Req)} of
    {'GET',<<"/metrics">>} -> {ok,[],prometheus_text_format:format()};
    _                      -> ignore
  end.

%% @doc On `elli_startup', register two metrics, a counter `http_requests_total'
%% and a histogram `http_request_duration_microseconds'. Then, on
%% `request_complete', {@link prometheus_counter:inc/2. increment}
%% `http_requests_total' and {@link prometheus_histogram:observe/3. observe}
%% `http_request_duration_microseconds'. Ignore all other events.
handle_event(request_complete, [Req,StatusCode,_Hs,_B,Timings], _Config) ->
  Method      = elli_request:method(Req),
  Handler     = case elli_request:path(Req) of
                  [H|_] -> H;
                  []    -> ""
                end,
  Labels      = [Method,Handler,StatusCode],
  prometheus_counter:inc(?TOTAL, Labels),
  prometheus_histogram:observe(?DURATION, Labels, duration(Timings)),
  ok;
handle_event(elli_startup, _Args, _Config) ->
  prometheus_counter:declare(metric(?TOTAL, ?LABELS, "request count")),
  prometheus_histogram:declare(metric(?DURATION, ?LABELS, "execution time")),
  ok;
handle_event(_Event, _Args, _Config) ->
  ok.

%%%===================================================================
%%% Private functions
%%%===================================================================

duration(Timings) ->
  UserStart = proplists:get_value(user_start, Timings, {0,0,0}),
  UserEnd   = proplists:get_value(user_end, Timings, {0,0,0}),
  timer:now_diff(UserEnd, UserStart).

metric(Name, Labels, Desc) ->
  [{name,Name},{labels,Labels},{help,"HTTP request "++Desc}].
