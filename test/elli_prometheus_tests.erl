-module(elli_prometheus_tests).

-include_lib("eunit/include/eunit.hrl").

-define(README, "README.md").

elli_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [{foreach,
     fun init_stats/0, fun clear_stats/1,
     [?_test(hello_world()),
      ?_test(sendfile())]}]}.

setup() ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  inets:start(),
  prometheus:start(),

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
  elli_prometheus:handle_event(elli_startup, false, false).

clear_stats(_) ->
  prometheus_registry:clear().

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

sendfile() ->
  {ok, Response} = httpc:request("http://localhost:3001/sendfile"),
  F              = ?README,
  {ok, Expected} = file:read_file(F),
  ?assertMatch(200, status(Response)),
  ?assertEqual([{"connection", "Keep-Alive"},
                {"content-length", integer_to_list(size(Expected))}],
               headers(Response)),
  ?assertEqual(binary_to_list(Expected), body(Response)),

  Labels = ['GET', <<"sendfile">>, "success"],

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

  ExpectedBodySize = size(Expected),
  ExpectedHeadersSize = 65,
  ExpectedSize = ExpectedBodySize + ExpectedHeadersSize,
  ?assertMatch({1, ExpectedSize}, summary_value(http_response_size_bytes,
                                                ["full" | Labels])),
  ?assertMatch({1, ExpectedHeadersSize}, summary_value(http_response_headers_size_bytes,
                                                       ["full" | Labels])),
  ?assertMatch({1, ExpectedBodySize}, summary_value(http_response_body_size_bytes,
                                                    ["full" | Labels])).

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
