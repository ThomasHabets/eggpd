%%%-------------------------------------------------------------------
%%% File    : peerp.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(peerp).

%% API
-export([start_link/2,
	 start/2,
	 state_new/2,
	 state_idle/3,
	 state_connect/3,
	 state_opensent/3,
	 announce_route/1,
	 withdraw_route/1,
	 fail/0,
	 stop/0,
	 reset/0,
	 clear/0,
	 test/0
	]).

-include("records.hrl").

%-record(state, {}).

%% API
announce_route(_Route) ->
    ok.
withdraw_route(_Route) ->
    ok.
fail() ->
    ok.
stop() ->
    ok.
reset() ->
    ok.
clear() ->
    ok.

start_link(RibP, Peer) ->
    spawn_link(?MODULE, state_new, [RibP, Peer]).

start(RibP, Peer) ->
    spawn(?MODULE, state_new, [RibP, Peer]).

%%
%% States
%%
state_reset(Parent, Peercp, Peer) ->
    peercp:stop(self(), Peer),
    state_reset_wait(Parent, Peercp, Peer).

state_reset_wait(Parent, Peercp, Peer) ->
    receive
	Any ->
	    io:format("Peerp> state_connected: unexpected ~p~n", [Any]),
	    state_reset_wait(Parent, Peercp, Peer)
    after 100 ->
	    state_new(Parent, Peer)
    end.

	    
state_new(Parent, Peer) ->
    Peercp = peercp:start(self(), Peer),
    link(Peercp),
    state_idle(Parent, Peercp, Peer).


state_idle(Parent, Peercp, Peer) ->
    receive
	{_From, stop} ->
	    exit(signallet)
    after 1000 ->
	    peercp:connect(Peercp),
	    state_connect(Parent, Peercp, Peer)
    end.

state_connect(Parent, Peercp, Peer) ->
    receive
	{Peercp, {ack, _}} ->                      % Ignore these
	    state_connect(Parent, Peercp, Peer);	    
	{Peercp, {ok, connected}} ->
	    peercp:send_open(Peercp),
	    state_opensent(Parent, Peercp, Peer);
	Any ->
	    io:format("Peerp> state_connected: unexpected ~p~n", [Any]),
	    state_reset(Parent, Peercp, Peer)
    end.

state_opensent(Parent, Peercp, Peer) ->
    receive
	{Peercp, {ack, _}} ->                      % Ignore these
	    state_opensent(Parent, Peercp, Peer);	    
	{Peercp, {open, _Msg}} ->
	    peercp:send_keepalive(Peercp),
	    state_openconfirm(Parent, Peercp, Peer);
	Any ->
	    io:format("Peerp> state_opensent: unexpected ~p~n", [Any])
    end.

state_openconfirm(Parent, Peercp, Peer) ->
    receive
	{Peercp, {ack, _}} ->                      % Ignore these
	    state_openconfirm(Parent, Peercp, Peer);	    
	{Peercp, keepalive} ->
	    state_established(Parent, Peercp, Peer);
	Any ->
	    io:format("Peerp> state_openconfirm: unexpected ~p~n", [Any])
    end.
    
state_established(Parent, Peercp, Peer) ->
    receive
	{Peercp, {ack, _}} ->                      % Ignore these
	    state_established(Parent, Peercp, Peer);	    
	{Peercp, {update, Msg}} ->
	    io:format("Peerp> Update ~p!!~n", [Msg]),
	    state_established(Parent, Peercp, Peer);
	{Peercp, keepalive} ->
	    state_established(Parent, Peercp, Peer);
	Any ->
	    io:format("Peerp> state_established: unexpected ~p~n", [Any])
    after 3000 ->
	    peercp:send_keepalive(Peercp),
	    state_established(Parent, Peercp, Peer)
    end.
    
test() ->
    Peer = #peer{ip="192.168.42.206",
		 as=65021},
    state_new(self(), Peer).
