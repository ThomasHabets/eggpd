%%%-------------------------------------------------------------------
%%% File    : eggpd_fib.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%   Interact with routing table. Currently doesn't do anything.
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
-module(eggpd_fib).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 add_route/1,
	 delete_route/1,
	 fail/0,
	 stop/0,
	 clear/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_route(Route)      -> gen_server:call(?MODULE, {add_route, Route}).
delete_route(Route)   -> gen_server:call(?MODULE, {delete_route, Route}).
fail()                -> gen_server:call(?MODULE, fail).
stop()                -> gen_server:call(?MODULE, stop).
clear()               -> gen_server:call(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_route, Route}, _From, State) ->
    io:format("FIBP> add route: ~p~n", [Route]),
    {reply, ok, State};

handle_call({delete_route, Route}, _From, State) ->
    io:format("FIBP> delete route: ~p~n", [Route]),
    {reply, ok, State};
    
handle_call(clear, _From, State) ->
    io:format("FIBP> stop~n"),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    io:format("FIBP> stop~n"),
    {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    io:format("FIBP> warning: unknown call ~p from ~p~n", [Request, From]),
    {stop, 'Invalid call', State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
