-module(elli_prometheus_test_callback).
-export([handle/2]).

-include_lib("elli/include/elli.hrl").

handle(Req, _Args) -> handle(Req#req.method, elli_request:path(Req), Req).

handle('GET', [<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response.
    {ok, [], <<"Hello World!">>};

handle('GET', [<<"hello">>], Req) ->
    %% Fetch a GET argument from the URL.
    Name = elli_request:get_arg(<<"name">>, Req, <<"undefined">>),
    {ok, [], <<"Hello ", Name/binary>>}.
