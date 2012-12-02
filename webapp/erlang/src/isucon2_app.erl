-module(isucon2_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = [{'_', [
                       {['_'], isucon2_index, []}]}],
    {ok, _} = cowboy:start_http(http, 100, [{port, 5000}], [{dispatch, Dispatch}]),
    isucon2_sup:start_link().

stop(_State) ->
    ok.
