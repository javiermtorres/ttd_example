-module(stream_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-record(state, {plc_app}).
-define(tick_interval, 500).

init(_Transport, Req, _Opts, _Active) ->
    io:format("bullet init~n"),
    Plc_app = ampel_app,
    Plots = [{a1, red}, {a1, yellow}, {a1, green}, 
             {a2, red}, {a2, yellow}, {a2, green}],
    Msgs = plc_viz:gen_svg_and_vars(Plc_app) ++ plc_viz:create_plots(Plots),
    [self() ! {?MODULE, msg, M} || M <- Msgs],
    ok = pg2:create(plc_update_receiver),
    ok = pg2:join(plc_update_receiver, self()),
    timer:send_interval(?tick_interval, tick),
    {ok, Req, #state{plc_app=Plc_app}}.

stream(<<"ping: ", Name/binary>>, Req, State) ->
    io:format("ping ~p received~n", [Name]),
    {reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
    io:format("stream received ~s~n", [Data]),
    {ok, Req, State}.

info({_, msg, Plist}=Msg, Req, State) ->
    Reply = jsx:encode(Plist),
    io:format("stream_handler: msg ~p, reply \"~s\"\n", [Msg, Reply]),
    {reply, Reply, Req, State};
info({_, fb_change_notification, Plist}=Msg, Req, State) ->
    Msgs = plc_viz:get_change_notifications(Plist),
    [self() ! {?MODULE, msg, M} || M <- Msgs],
    io:format("stream_handler: change_notification ~p, resend ~p\n",
              [Msg, Msgs]),
    {ok, Req, State};
info({_, fb_event_notification, Plist}=Msg, Req, State) ->
    Msgs = plc_viz:get_event_notifications(Plist),
    [self() ! {?MODULE, msg, M} || M <- Msgs],
    io:format("stream_handler: event_notification ~p, resend ~p\n",
              [Msg, Msgs]),
    {ok, Req, State};
info(tick, Req, State) ->
    Reply = jsx:encode(svg_json:tick()),
    %% io:format("stream_handler: tick \"~s\"\n", [Reply]),
    {reply, Reply, Req, State};
info(Info, Req, State) ->
    io:format("stream_handler: info spurious message ~p\n", [Info]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    io:format("bullet terminate~n"),
    ok.
