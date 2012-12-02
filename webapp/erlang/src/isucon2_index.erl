-module(isucon2_index).
-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Content}  = isucon2_index_dtl:render([{homepage, foobar}]),
%%    io:format("content: ~p~n", [Content]),
    {ok, Body} = isucon2_base_dtl:render([{content, Content}]),
%%    io:format("body: ~p~n", [Body]),
    {ok, Req2}  = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
