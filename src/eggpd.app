%%% -*- erlang *-*
%%%-------------------------------------------------------------------
%%% File    : eggpd.app
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description : 
%%%   .app file for EGG BGP Daemon.
%%%   Add any new process names and modules here.
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
{application, eggpd, 
 [{description, "EGG BGPD"},
  {vsn, "0.01"},
  {modules, [eggpd,
  	     eggpd_app,
	     eggpd_connection,
	     eggpd_fibp,
	     eggpd_peer,
	     eggpd_peer_sup,
	     eggpd_ribp,
	     eggpd_sup]},
  {registered,[eggpd_peer_sup,
	       eggpd_sup,
	       eggpd_ribp,
	       eggpd_fibp]},
  {applications, [kernel, stdlib]},
  {mod, {eggpd_app,[]}},
  {start_phases, []}
 ]}.
