%%%-------------------------------------------------------------------
%%% File    : eggpd_peer.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%   Peer state machine.
%%%
%%% Copyright :
%%% Copyright 2008,2011 Thomas Habets <thomas@habets.se>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(eggpd_peer).
-behaviour(gen_fsm).

%% Startup/tairdown/codechange
-export([init/1,
	 start_link/1,
	 terminate/3,

	 %% States
	 idle/2,
	 %% active/2,
	 connect/2,
	 open_sent/2,
	 open_confirm/2,
	 established/2,

	 %% Callbacks
	 handle_event/3,
	 handle_sync_event/4]).

-include("records.hrl").

%%--------------------------------------------------------------------
%% Startup/tairdown/codechange

%
start_link(Peer) ->
    PeerConfig = #peer{ip=Peer, as=65020, localas=65021},
    gen_fsm:start_link(?MODULE, [PeerConfig], []).

init([PeerConfig]) ->
    io:format("--- PEER INIT ~p ---~n", [self()]),
    {ok, _} = timer:apply_after(10000,
				gen_fsm, send_event,
				[self(), automatic_start]),
    {ok, idle, #peer_state{peer=PeerConfig}}.

%% Callbacks
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_sync_event(get_event, _From, State, Data) ->
    io:format("peer(~p): ------> Sync event~n", [self()]),
    {reply, {State, Data}, State, Data}.

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
open_sent({open, Options}, State) ->
    io:format("peer(~p): open_sent -> open_confirm~n", [self()]),
    io:format("peer(~p): open options: ~p~n", [self(), Options]),
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


stop(Pid) ->
    gen_fsm:send_all_state_event(Pid, stop).

terminate(Reason, State, Data) ->
    io:format("peer(~p): terminate(~p, ~p, ~p)~n",
	      [self(), Reason, State, Data]).
