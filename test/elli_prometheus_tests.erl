-module(elli_prometheus_tests).

-include_lib("eunit/include/eunit.hrl").

elli_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [{foreach,
     fun init_stats/0, fun clear_stats/1,
     [?_test(hello_world())]}]}.

setup() ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  inets:start(),
  prometheus:start(),

  %% force elli_middleware module loading
  %% otherwise elli won't recognize it as a callback
  %% when erlang runs in interactive mode.
  elli_middleware:module_info(),
  Config = [
            {mods, [
                    {elli_prometheus, []},
                    {elli_example_callback, []}
                   ]}
           ],

  {ok, P} = elli:start_link([{callback, elli_middleware},
                             {callback_args, Config},
                             {port, 3001}]),
  unlink(P),
  [P].

teardown(Pids) ->
  [elli:stop(P) || P <- Pids].

init_stats() ->
  ets:new(elli_stat_table, [set, named_table, public]).

clear_stats(_) ->
  ets:delete(elli_stat_table).

histogram_count_sum(Name, Labels) ->
  {Buckets, Sum} = prometheus_histogram:value(Name, Labels),
  {lists:sum(Buckets), Sum}.

counter_value(Name, Labels) ->
  prometheus_counter:value(Name, Labels).

summary_value(Name, Labels) ->
  prometheus_summary:value(Name, Labels).

hello_world() ->
  {ok, Response} = httpc:request("http://localhost:3001/hello/world"),
  ?assertMatch(200, status(Response)),
  ?assertMatch([{"connection", "Keep-Alive"},
                {"content-length", "12"}], headers(Response)),
  ?assertMatch("Hello World!", body(Response)),
  %%?debugVal(ets:tab2list(prometheus_histogram_table)),
  Labels = ['GET', <<"hello">>, "success"],
  ?assertMatch(1, counter_value(http_requests_total, Labels)),
  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_request_duration_microseconds,
                                               ["full" | Labels])),
  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_request_headers_microseconds,
                                               Labels)),
  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_request_body_microseconds,
                                               Labels)),
  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_request_user_microseconds,
                                               Labels)),

  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_response_send_microseconds,
                                               ["full" | Labels])),
  ?assertMatch({1, 75}, summary_value(http_response_size_bytes,
                                      ["full" | Labels])),
  ?assertMatch({1, 63}, summary_value(http_response_headers_size_bytes,
                                      ["full" | Labels])),
  ?assertMatch({1, 12}, summary_value(http_response_body_size_bytes,
                                      ["full" | Labels])).

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
