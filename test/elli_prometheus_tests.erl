-module(elli_prometheus_tests).

-include_lib("eunit/include/eunit.hrl").

-define(README, "README.md").

-define(EMPTY_SCRAPE_TEXT,
        "# TYPE http_client_closed_total counter
# HELP http_client_closed_total HTTP request \"client_closed\" errors count
# TYPE http_requests_failed_total counter
        # HELP http_requests_failed_total HTTP request failed total count.
# TYPE http_client_timeout_total counter
# HELP http_client_timeout_total HTTP request \"client_timeout\" errors count
# TYPE http_bad_requests_total counter
# HELP http_bad_requests_total HTTP request \"bad_request\" errors count
# TYPE http_requests_total counter
# HELP http_requests_total HTTP request request count
# TYPE http_request_duration_microseconds histogram
# HELP http_request_duration_microseconds HTTP request  latencies in microseconds
# TYPE http_request_headers_microseconds histogram
# HELP http_request_headers_microseconds HTTP request time spent receiving and parsing headers
# TYPE http_request_user_microseconds histogram
# HELP http_request_user_microseconds HTTP request time spent in user callback
# TYPE http_response_send_microseconds histogram
# HELP http_response_send_microseconds HTTP request time spent sending reply
# TYPE http_request_body_microseconds histogram
# HELP http_request_body_microseconds HTTP request time spent receiving and parsing body
# TYPE http_response_body_size_bytes summary
# HELP http_response_body_size_bytes HTTP request response body size
# TYPE telemetry_scrape_duration_seconds summary
# HELP telemetry_scrape_duration_seconds Scrape duration
# TYPE http_response_size_bytes summary
# HELP http_response_size_bytes HTTP request total response size
# TYPE telemetry_scrape_size_bytes summary
# HELP telemetry_scrape_size_bytes Scrape size, uncompressed
# TYPE http_response_headers_size_bytes summary
# HELP http_response_headers_size_bytes HTTP request response headers size

").

-define(EMPTY_SCRAPE_SIZE, 1762).

normalize_text_scrape(Scrape) ->
  lists:sort(lists:map(fun string:strip/1, string:tokens(Scrape, "\n"))).

elli_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [{foreach,
     fun init_stats/0, fun clear_stats/1,
     [?_test(hello_world()),
      ?_test(scrape()),
      ?_test(scrape_neg()),
      ?_test(sendfile()),
      ?_test(chunked()),
      ?_test(bad_request_line()),
      ?_test(too_many_headers()),
      ?_test(way_too_big_body())]}]}.

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

scrape() ->
  {ok, Response} = httpc:request("http://localhost:3001/metrics"),
  ?assertMatch(200, status(Response)),
  ExpectedCL = integer_to_list(?EMPTY_SCRAPE_SIZE),
  CT = prometheus_text_format:content_type(),
  ExpectedCT = binary_to_list(CT),
  ?assertMatch([{"connection", "Keep-Alive"},
                {"content-length", ExpectedCL},
                {"content-type", ExpectedCT}], headers(Response)),
  ?assertEqual(normalize_text_scrape(?EMPTY_SCRAPE_TEXT),
               normalize_text_scrape(body(Response))),

  ExpectedSCount = 1,

  ?assertMatch({ExpectedSCount, ?EMPTY_SCRAPE_SIZE},
               summary_value(telemetry_scrape_size_bytes,
                             [default, CT])),

  {SCount, SDuration} = summary_value(telemetry_scrape_duration_seconds,
                                      [default, CT]),

  ?assertMatch(ExpectedSCount, SCount),
  ?assertEqual(true, SDuration > 0 andalso SDuration < 0.01).

scrape_neg() ->
  Accept = "application/vnd.google.protobuf;"
    "proto=io.prometheus.client.MetricFamily;encoding=delimited;q=0.7,"
    "text/plain;version=0.0.4;q=0.3,"
    "application/json;schema=\"prometheus/telemetry\";version=0.0.2;q=0.2,"
    "*/*;q=0.1",
  {ok, Response} = httpc:request(get, {"http://localhost:3001/metrics",
                                       [{"Accept", Accept}]}, [], []),
  ?assertMatch(200, status(Response)),
  CT = prometheus_protobuf_format:content_type(),
  ExpectedCT = binary_to_list(CT),
  ?assertMatch([{"connection", "Keep-Alive"},
                {"content-length", _},
                {"content-type", ExpectedCT}], headers(Response)).

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

chunked() ->
  Expected = "chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1",

  {ok, Response} = httpc:request("http://localhost:3001/chunked"),

  ?assertMatch(200, status(Response)),
  ?assertEqual([{"connection", "Keep-Alive"},
                %% httpc adds a content-length, even though elli
                %% does not send any for chunked transfers
                {"content-length", integer_to_list(length(Expected))},
                {"content-type", "text/event-stream"}], headers(Response)),
  ?assertMatch(Expected, body(Response)),

  Labels = ['GET', <<"chunked">>, "success"],

  ?assertMatch(1, counter_value(http_requests_total, Labels)),
  ?assertMatch({1, S} when S > 0,
                           histogram_count_sum(http_request_duration_microseconds,
                                               ["chunks" | Labels])),
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
                                               ["chunks" | Labels])),

  ExpectedBodySize = 111, %% size(Expected) + encoding overhead
  ExpectedHeadersSize = 104,
  ExpectedSize = ExpectedBodySize + ExpectedHeadersSize,
  ?assertMatch({1, ExpectedSize}, summary_value(http_response_size_bytes,
                                                ["chunks" | Labels])),
  ?assertMatch({1, ExpectedHeadersSize}, summary_value(http_response_headers_size_bytes,
                                                       ["chunks" | Labels])),
  ?assertMatch({1, ExpectedBodySize}, summary_value(http_response_body_size_bytes,
                                                    ["chunks" | Labels])).

bad_request_line() ->
  {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                 [{active, false}, binary]),

  Req = <<"FOO BAR /hello HTTP/1.1\r\n">>,
  gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
  ?assertMatch({ok, <<"HTTP/1.1 400 Bad Request\r\n"
                      "Content-Length: 11\r\n\r\nBad Request">>},
               gen_tcp:recv(Socket, 0)),

  Labels = [request_parse_error],

  ?assertMatch(1, counter_value(http_requests_failed_total, Labels)).

too_many_headers() ->
  Headers = lists:duplicate(100, {"X-Foo", "Bar"}),
  {ok, Response} = httpc:request(get, {"http://localhost:3001/foo", Headers},
                                 [], []),
  ?assertMatch(400, status(Response)),

  ?assertMatch(1, counter_value(http_bad_requests_total, [too_many_headers])),
  ?assertMatch(1, counter_value(http_requests_failed_total, [bad_request])).

way_too_big_body() ->
  Body = binary:copy(<<"x">>, (1024 * 2000) + 1),
  ?assertMatch({error, socket_closed_remotely},
               httpc:request(post,
                             {"http://localhost:3001/foo", [], [], Body},
                             [], [])),


  ?assertMatch(1, counter_value(http_bad_requests_total, [body_size])),
  ?assertMatch(1, counter_value(http_requests_failed_total, [bad_request])).


%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
