-module(routemon).

-export([start/0]).

-include("records.hrl").

print_routes(_P, _Attr, []) ->
    true;
print_routes(P, Attr, [H|T]) ->
    print_route(P,Attr,H).

route_to_str({Net, Len}) ->
    A=Net bsr 24,
    B=(Net bsr 16) band 255,
    C=(Net bsr 8) band 255,
    D=Net band 255,
    peercp:fmt(["~p.~p.~p.~p/~p", A,B,C,D,Len]).

pathattr_extract_aspath(A) ->
    fixme.

print_route(P, Attr, R) ->
    io:format("~p~p ~p~n", [P,
			    route_to_str(R),
			    pathattr_extract_aspath(Attr)]).

loop(Peerp) ->
    receive
	{From, {received_routes, {Attr, Routes}}} ->
	    print_routes('adding ', Attr, Routes),
	    loop(Peerp);
	{From, {withdrawn_routes, {Attr, Routes}}} ->
	    print_routes('removing ', Attr, Routes),
	    loop(Peerp);
	Any ->
	    io:format("Unexpected ~p...~n", [Any]),
	    loop(Peerp)
    end.

init() ->
    _Peer1 = #peer{ip="192.168.42.206",
		   as=65021,
		   passive=true},
    _Peer2 = #peer{ip="192.168.42.118",
		   as=65501},
    _Peer3 = #peer{ip="217.118.211.82",
		   as=65025,
		   localas=65025,
		   localport=0},
    Peer = _Peer3,
    peerp:start_link(self(), Peer).

start() ->
    loop(init()).
