%%%-------------------------------------------------------------------
%%% File    : eggpd_sup.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.se>
%%%-------------------------------------------------------------------
-module(eggpd_peer_sup).
-behaviour(supervisor).

-export([
	 interactive/0,
	 start_link/0,
	 init/1]).

%% Start from shell
interactive() ->
    supervisor:start({local,?MODULE}, ?MODULE, []).

%% Start with parameters
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

%% Init supervisor. What subprocesses to monitor
init([]) ->
    Processes = [
		 {eggpd_peer,
		  {eggpd_peer, start_link, ["192.168.42.4"]},
		  permanent, 
		  10000, 
		  worker, 
		  dynamic}
		],
    {ok,
     {{one_for_one, 3, 10},
      Processes}
    }.
