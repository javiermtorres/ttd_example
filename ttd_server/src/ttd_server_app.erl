%%%-------------------------------------------------------------------
%% @doc ttd_server public API
%% @end
%%%-------------------------------------------------------------------

-module('ttd_server_app').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_',
                   [{"/", cowboy_static, {priv_file, ttd_server, "index.html"}},
                    {"/bullet", bullet_handler, [{handler, stream_handler}]},
                    {"/[...]", cowboy_static, {priv_dir, ttd_server, []}}]}]
                ),
        {ok, _} = cowboy:start_http(http, 10,
                                    [{port, 1234}],
                                    [{env, [{dispatch, Dispatch}]}]),
    'ttd_server_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
