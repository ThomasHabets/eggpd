%%%-------------------------------------------------------------------
%%% File    : eggpd_super.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(eggpd_super).
-behaviour(supervisor).

-export([start/0,
	 interactive/0,
	 start_link/1,
	 init/1]).

%% Start "as deamon"
start() ->
    spawn(fun() ->
	     supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).

%% Start from shell
interactive() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).

%% Start with parameters
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%% Init supervisor. What subprocesses to monitor
init([]) ->
    {ok,
     {
       %% Flags
       {one_for_one, 3, 10},

       %% Children
       [{ribp, 
	 {ribp, start_link, []},
	 permanent,
	 10000,                     % Shutdown time
	 worker,
	 [ribp]},
	{fibp, 
	 {fibp, start_link, []},
	 permanent, 
	 10000, 
	 worker, 
	 [fibp]}
       ]}}.
