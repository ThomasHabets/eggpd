%%%-------------------------------------------------------------------
%%% File    : peerp.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(peerp).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start/0,
	 announce_route/1,
	 withdraw_route/1,
	 fail/0,
	 stop/0,
	 reset/0,
	 clear/0
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

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
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

announce_route(Route) -> gen_server:call(?MODULE, {announce_route, Route}).
withdraw_route(Route) -> gen_server:call(?MODULE, {withdraw_route, Route}).
reset()               -> gen_server:call(?MODULE, reset).
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


%% state_connected() ->
%%     io:format("Peerp: state=connected~n"),
%%     receive
%% 	{tcp, Socket, Bin} ->
%% 	    io:format("Got: ~w  ~w~n", Socket, Bin),
%% 	    ?MODULE:state_connected();
%% 	{tcp_closed, _Socket, _Bin} ->
%% 	    ?MODULE:state_initial()
%%     end.

%% do_listen() ->
%%     {ok, Listen} = gen_tcp:listen(179, [ binary, {packet, 4},
%% 					 {reuseaddr, true},
%% 					 {active, true}]),
%%     {ok, Socket} = get_tcp:accept(Listen),
%%     Socket.
%% state_initial() ->
%%     io:format("Peerp: state=initial~n"),
%%     {ok, Socket} = gen_tcp:connect('192.168.42.253', 179,
%% 				  [binary, {packet, 0}]),
%%     receive
%% 	{tcp, Socket, Bin} ->
%% 	    io:format("Got: ~w  ~w~n", Socket, Bin)
%%     end,
%%     state_connected().

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({announce_route, Route}, _From, State) ->
    io:format("PEERP> announce route: ~p~n", [Route]),
    {reply, ok, State};

handle_call({withdraw_route, Route}, _From, State) ->
    io:format("PEERP> withdraw route: ~p~n", [Route]),
    {reply, ok, State};
    
handle_call(clear, _From, State) ->
    io:format("PEERP> stop~n"),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    io:format("PEERP> stop~n"),
    {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    io:format("PEERP> warning: unknown call ~p from ~p~n", [Request, From]),
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
