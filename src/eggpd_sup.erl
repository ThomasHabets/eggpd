%%%-------------------------------------------------------------------
%%% File    : eggpd_sup.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%   eggpd root supervisor.
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
    Processes = [{eggpd_ribp,
		  {eggpd_ribp, start_link, []},
		  permanent,
		  10000,                     % Shutdown time
		  worker,
		  dynamic},
		 {eggpd_fibp,
		  {eggpd_fibp, start_link, []},
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
