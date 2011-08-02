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
	 add_peer/1,
	 init/1]).

%% Start from shell
interactive() ->
    supervisor:start({local,?MODULE}, ?MODULE, []).

%% Start with parameters
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

add_peer(IP)  ->
    supervisor:start_child(?MODULE,
			   {eggpd_peer,
			    {eggpd_peer, start_link, [IP]},
			    permanent, 
			    10000, 
			    worker, 
			    dynamic}).

%% Init supervisor. What subprocesses to monitor
init([]) ->
    Processes = [],
    {ok,
     {{one_for_one, 3, 10},
      Processes}
    }.
