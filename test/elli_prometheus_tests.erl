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

hello_world() ->
  {ok, Response} = httpc:request("http://localhost:3001/hello/world"),
  ?assertMatch(200, status(Response)),
  ?assertMatch([{"connection", "Keep-Alive"},
                {"content-length", "12"}], headers(Response)),
  ?assertMatch("Hello World!", body(Response)).

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
