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
