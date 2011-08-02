%%%-------------------------------------------------------------------
%%% File    : eggpd_sup.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.se>
%%%-------------------------------------------------------------------
-module(eggpd_sup).
-behaviour(supervisor).

-export([
	 interactive/0,
	 start_link/1,
	 init/1]).

%% Start from shell
interactive() ->
    supervisor:start({local,?MODULE}, ?MODULE, _Arg = []).

%% Start with parameters
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%% Init supervisor. What subprocesses to monitor
init([]) ->
    Processes = [{ribp, 
		  {ribp, start_link, []},
		  permanent,
		  10000,                     % Shutdown time
		  worker,
		  dynamic},
		 {fibp, 
		  {fibp, start_link, []},
		  permanent, 
		  10000, 
		  worker, 
		  dynamic},
		 {eggpd_peer_sup,
		  {eggpd_peer_sup, start_link, []},
		  permanent, 
		  10000, 
		  supervisor, 
		  dynamic}
		],
    {ok,
     {{one_for_one, 3, 10},
      Processes}
    }.
