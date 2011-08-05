%%%-------------------------------------------------------------------
%%% File    : eggpd_peer_sup.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
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
-module(eggpd_peer_sup).
-behaviour(supervisor).

-export([start_link/0,
	 add_peer/1,
	 init/1]).

%% Start with parameters
start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

% API
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
