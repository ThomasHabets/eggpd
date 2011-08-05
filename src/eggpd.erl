%%%-------------------------------------------------------------------
%%% File    : eggpd.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description :
%%%   Start up and shut down application.
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
-module(eggpd).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
	    ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(crypto),
    %appmon:start(),
    application:start(eggpd).

stop() ->
    application:stop(eggpd).
