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
	 %handle_info/3,
	 handle_sync_event/4
	 %terminate/3
	]).
-include("records.hrl").

%-record(state, {}).

% debug
    
handle_sync_event(get_event, _From, State, Data) ->
    io:format("------> Sync event~n"),
    {reply, {State, Data}, State, Data}.

%% API
start_link(Peer) ->
    PeerConfig = #peer{ip=Peer, as=65020, localas=65021},
    erlang:process_flag(trap_exit, true),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [PeerConfig], []).

init([PeerConfig]) ->
    Data = #peer_state{peer=PeerConfig},
    {ok, _} = timer:apply_after(1000,
				gen_fsm,send_event,
				[eggpd_peer, automatic_start]),
    {ok, idle, Data}.


%% Helper function
go_state_idle(State) ->
    eggpd_connection:stop(State#peer_state.connection),
    {next_state, idle, State#peer_state{connection=void}}.

%%
%% States and their messages (RFC4271 8.2.2)
%%
idle(automatic_start, State) ->
    io:format("Idle, about to change to connect~n"),
    Con=eggpd_connection:start_link(State#peer_state.peer),
    eggpd_connection:connect(Con),
    State1 = State#peer_state{connection=Con},
    {next_state, connect, State1}.
    
connect(manual_stop, State) ->
    go_state_idle(State);

%% DelayOpen not implemented
%% TODO: what is the right event name for this?
connect(connected, State) ->
    eggpd_connection:send_open(State#peer_state.connection),
    {next_state, open_sent, State}.

%% TODO: what is the right event name for this?
open_sent({open, _Options}, State) ->
    eggpd_connection:send_keepalive(State#peer_state.connection),
    {next_state, open_confirm, State}.

open_confirm(keepalive, State) ->
    {next_state, established, State}.

established({notification, Data}, State) ->
    io:format("Notification: ~p~n", [Data]),
    go_state_idle(State);

established(keepalive, State) ->
    %%io:format("PEER Keepalive~n"),
    eggpd_connection:send_keepalive(State#peer_state.connection),
    {next_state, established, State};

established({update, Data}, State) ->
    io:format("Update: ~p~n", [Data]),
    {next_state, established, State}.

% TODO: state active

%%
%%
%%
print_info({Net, Len}) ->
    A=Net bsr 24,
    B=(Net bsr 16) band 255,
    C=(Net bsr 8) band 255,
    D=Net band 255,
    io:format("    ~p.~p.~p.~p/~p~n", [A,B,C,D,Len]).


code_change(_OldVersion, State, Data, _Extra) ->
    io:format("Code change: ~p ~p~n", [State, Data]),
    {ok, Data}.

stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

terminate(Reason, State, Data) ->
    io:format("PEER Terminate(~p, ~p, ~p)~n", [Reason, State, Data]),
    ok.
handle_info(Info, State, Data) ->
    io:format("PEER Got info: ~p~n", [Info]),
    go_state_idle(State).
