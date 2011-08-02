%%%-------------------------------------------------------------------
%%% File    : eggpd_peer.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%   Test with eggpd_peer:test().
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(eggpd_peer).
-behaviour(gen_fsm).
%% API
-compile(export_all).

-export([
	 init/1,
	 start_link/1,
	 code_change/4,
	 handle_event/3,
	 %% handle_info/3,   % Ignore warning about missing callback
	 handle_sync_event/4
	 %% terminate/3      % Ignore warning about missing callback
	]).
-include("records.hrl").

%-record(state, {}).

% debug
    
handle_sync_event(get_event, _From, State, Data) ->
    io:format("peer(~p): ------> Sync event~n", [self()]),
    {reply, {State, Data}, State, Data}.

%% API
start_link(Peer) ->
    PeerConfig = #peer{ip=Peer, as=65020, localas=65021},
    gen_fsm:start_link(?MODULE, [PeerConfig], []).

init([PeerConfig]) ->
    io:format("--- PEER INIT ~p ---~n", [self()]),
    {ok, _} = timer:apply_after(10000,
				gen_fsm, send_event,
				[self(), automatic_start]),
    {ok, idle, #peer_state{peer=PeerConfig}}.

%%
%% States and their messages (RFC4271 8.2.2)
%%
idle(automatic_start, State) ->
    io:format("peer(~p): idle -> connect~n", [self()]),
    Con=eggpd_connection:start_link(State#peer_state.peer),
    eggpd_connection:connect(Con),
    State1 = State#peer_state{connection=Con},
    {next_state, connect, State1}.
    
%% DelayOpen not implemented
%% TODO: what is the right event name for this?
connect(connected, State) ->
    io:format("peer(~p): connect -> open_sent~n", [self()]),
    eggpd_connection:send_open(State#peer_state.connection),
    {next_state, open_sent, State}.

%% TODO: what is the right event name for this?
open_sent({open, _Options}, State) ->
    io:format("peer(~p): open_sent -> open_confirm~n", [self()]),
    eggpd_connection:send_keepalive(State#peer_state.connection),
    {next_state, open_confirm, State}.

open_confirm(keepalive, Data) ->
    io:format("peer(~p): open_confirm -> established~n", [self()]),
    {next_state, established, Data}.

established({notification, Notification}, Data) ->
    io:format("peer(~p): notification: ~p~n",
	      [self(), Notification]),
    {stop, notification, Data};

established(keepalive, Data) ->
    io:format("peer(~p): keepalive~n", [self()]),
    eggpd_connection:send_keepalive(Data#peer_state.connection),
    {next_state, established, Data};

established({update,
	     Update={withdraw, Withdraw,
		     pathattr, _Pathattr,
		     info, Announce}}, Data) ->
    io:format("peer(~p): update: ~p~n", [self(), Update]),
    io:format("peer(~p): withdraw:~n", [self()]),
    print_info_block(Withdraw),
    io:format("peer(~p): announce:~n", [self()]),
    print_info_block(Announce),
    {next_state, established, Data}.

% TODO: state active

%%
%%
%%
print_info_block([]) ->
    ok;
print_info_block([H|T]) ->
    print_info(H),
    print_info_block(T).

print_info({Net, Len}) ->
    A=Net bsr 24,
    B=(Net bsr 16) band 255,
    C=(Net bsr 8) band 255,
    D=Net band 255,
    io:format("peer(~p):    ~p.~p.~p.~p/~p~n", [self(), A,B,C,D,Len]).


code_change(_OldVersion, State, Data, _Extra) ->
    io:format("peer(~p), Code change: ~p ~p~n", [self(), State, Data]),
    {ok, Data}.

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

terminate(Reason, State, Data) ->
    io:format("peer(~p): terminate(~p, ~p, ~p)~n",
	      [self(), Reason, State, Data]).
