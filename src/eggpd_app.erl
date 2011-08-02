%%%-------------------------------------------------------------------
%%% File    : eggpd_app.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
%%
%% 1> application:load(eggpd).
%% ok
%% 2> application:start(eggpd).
%% ok
%% 3> ribp:add_route(123).
%% RIBP> add route: 123
%% ok
%% 4> appmon:start().

-module(eggpd_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------

start(_Type, StartArgs) ->
    eggpd_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------

stop(_State) ->
    ok.
