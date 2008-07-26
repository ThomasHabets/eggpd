%%%-------------------------------------------------------------------
%%% File    : peercp.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description :
%%%  c(peercp).
%%%  peercp:test().
%%%
%%% Created :  26 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(peercp).

-export([start/2,
	 test/0,
	 state_new/2,
	 state_idle/2,
	 state_idle/3,
	 state_opensent/3,
	 state_connect/2,
	 state_active/3,
	 state_established/3,
	 state_openconfirm/3,
	 fmt/1]).

-include("records.hrl").

fmt(X) ->
    [F|T] = X,
    lists:flatten(io_lib:format(F, T)).

info(P, X) ->
    io:format("~p> INFO ~p~n", [P,fmt(X)]).

warning(P, X) ->
    io:format("~p> WARNING ~p~n", [P,fmt(X)]).

debug(P, X) ->
    io:format("~p> DEBUG ~p ~n", [P,fmt(X)]).

state_new(Peerp, Peer) ->
    debug("PEERCP", ["State: new"]),
    Peerp ! {self(), started},
    state_idle(Peerp, Peer).

state_idle(Sock, Peerp, Peer) ->
    gen_tcp:close(Sock),
    ?MODULE:state_idle(Peerp, Peer).

state_idle(Peerp, Peer) ->
    debug("PEERCP", ["State: idle"]),
    timer:sleep(1000),
    ?MODULE:state_connect(Peerp, Peer).

%%
%%
%%
state_connect(Peerp, Peer) ->
    debug("PEERCP", ["State: connect"]),
    {ok, Sock} = gen_tcp:connect(Peer#peer.ip, 179, [binary, {packet, 0}]),
    debug("PEERCP", ["Sending OPEN... ~p", Sock]),
    _BGP_Length = 29,
    _BGP_AS = 16#ffaf,
    _BGP_Holdtime = 180,
    _BGP_Identifier = 16#12345678,
    _BGP_Opt_Parm_Len = 0,
    _BGP_Unused = 0,
    _BGP_Rest = <<>>,
    Head = ?BGP_OPEN_FMT,
    gen_tcp:send(Sock, Head),
    ?MODULE:state_opensent(Sock, Peerp, Peer).

%%
%%
%%
state_opensent_parse_open(_Sock, _Peerp, _Peer, OpenMsg) ->
    ?BGP_OPEN_FMT = OpenMsg,
    debug("PEERCP", ["BGP Ident: ~p~n", _BGP_Identifier]),
    case _BGP_AS of
	65021 ->
	    {ok, _BGP_AS};
	_ ->
	    warning("PEERCP", ["Wrong ASN: ~p~n", _BGP_AS]),
	    {error, bad_as}
    end.


%%
%%
%%
state_opensent(Sock, Peerp, Peer) ->
    debug("PEERCP", ["State: OpenSent"]),
    receive
	{From, status} ->
	    info("PEERCP", ["FIXME: status"]),
	    From ! {self(), "FIXME: status"};
	{stop, Reason} ->
	    info("PEERCP", ["Stop: ~p", Reason]);
	{tcp_closed, Sock} ->
	    ?MODULE:state_idle(Sock, Peerp, Peer);
	%% Expected message
	{tcp, Sock, Msg} ->
	    {ok, _} = state_opensent_parse_open(Sock, Peerp, Peer, Msg),
	    gen_tcp:send(Sock, ?BGP_KEEPALIVE_FMT),
	    ?MODULE:state_openconfirm(Sock, Peerp, Peer);
	Any ->
	    warning("PEERCP", ["Invalid message: ~p, continuing...", Any]),
	    ?MODULE:state_opensent(Sock, Peerp, Peer)
    after 3000 ->
	    exit(reason)
    end.

iskeepalive(Msg) ->
    {ok, Msg}.
%%
%%
%%
state_openconfirm(Sock, Peerp, Peer) ->
    debug("PEERCP", ["State: openconfirm"]),
    receive 
	%% Expected message
	{tcp, Sock, Msg} ->
	    {ok, _} = iskeepalive(Msg),
	    ?MODULE:state_established(Sock, Peerp, Peer);
	{From, status} ->
	    From ! "Status: FIXME",
	    ?MODULE:state_openconfirm(Sock, Peerp, Peer);
	Any ->
	    warning("PEERCP", ["Invalid message: ~p, continuing...", Any]),
	    ?MODULE:state_openconfirm(Sock, Peerp, Peer)
    after 3000 ->
	    ?MODULE:state_idle(Sock, Peerp, Peer)
    end.

%%
%%
%%
state_active(Sock, Peerp, Peer) ->
    debug("PEERCP", ["State: active"]),
    receive 
	{From, status} ->
	    From ! "Status: FIXME",
	    ?MODULE:state_active(Sock, Peerp, Peer);
	Any ->
	    warning("PEERCP", ["Invalid message: ~p", Any])
    end.


%%
%%
%%
state_parse_msg(Msg) ->
    ?BGP_HEADER_FMT = Msg,
    case _BGP_Head_Type of
	?BGP_TYPE_OPEN ->
	    {open, Msg};
	?BGP_TYPE_KEEPALIVE ->
	    {keepalive, Msg};
	?BGP_TYPE_UPDATE ->
	    {update, Msg};
	?BGP_TYPE_NOTIFICATION ->
	    {notification, Msg};
	Other ->
	    warning('PEERCP', ["Unknown BGP message type: ~p", Other])
    end.
    
%%
%%
%%
state_established(Sock, Peerp, Peer) ->
    debug("PEERCP", ["State: established"]),
    receive 
	{From, status} ->
	    From ! "Status: FIXME",
	    ?MODULE:state_established(Sock, Peerp, Peer);
	{tcp, Sock, Msg} ->
	    Parsed = state_parse_msg(Msg),
	    debug('PEERCP', ["Parsed message: ~p", Parsed]),
	    case Parsed of
		{keepalive, _} ->
		    {ok, normal};
		{open, _} ->
		    {ok, normal};
		{update, _} ->
		    {ok, normal};
		{notification, _} ->
		    {ok, normal}
	    end,
	    ?MODULE:state_established(Sock, Peerp, Peer);
	{tcp_closed, Sock} ->
	    info('PEERCP', ["Connection reset by peer"]),
	    ?MODULE:state_idle(Sock, Peerp, Peer);
	Any ->
	    warning("PEERCP", ["Invalid message: ~p", Any]),
	    ?MODULE:state_established(Sock, Peerp, Peer)
    after 1000 ->
	    check_send_keepalive(Sock),
	    ?MODULE:state_established(Sock, Peerp, Peer)
    end.

%%
%%
%%
send_keepalive(Sock) ->
    debug("PEERCP", ["Sending keepalive"]),
    gen_tcp:send(Sock, ?BGP_KEEPALIVE_FMT).
    
%%
%% FIXME: Don't send too often
%%
check_send_keepalive(Sock) ->
    send_keepalive(Sock).

testloop(Peercp, Peer) ->
    receive
	Any ->
	    info("PEERCP-TEST", ["Msg from peercp: ~p", Any]),
	    ?MODULE:testloop(Peercp, Peer)
    end.

test() ->
    Peer = #peer{ip="192.168.42.206",
		 as=65021},
    Peercp = start(self(), Peer),

    receive
	{Peercp, started} ->
	    info("PEERCP-TEST", ["Msg from peercp ~p: started", Peercp]),
	    testloop(Peercp, Peer)
    after 1000 ->
	    warning("PEERCP-TEST", ["Timeout waiting for startup msg"])
    end,
    testloop(Peercp, Peer).

start(Peerp, Peer) ->
    info("PEERCP", ["Starting ~w ~w", Peerp, Peer]),
    spawn_link(?MODULE, state_new, [Peerp, Peer]).
