%%%-------------------------------------------------------------------
%%% File    : peercp.erl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description :
%%%  c(peercp).
%%%  peercp:test().
%%%
%%% Created :  26 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
-module(peercp).

%%
%% API
%%
-export([start/2,
	 stop/1,
	 connect/1,
	 send_open/1,
	 send_keepalive/1,
	 announce_route/3
	 ]).

%% Internal
-export([state_idle/2,
	 state_connected/3,
	 fmt/1]).

-include("records.hrl").

fmt(X) ->
    [F|T] = X,
    lists:flatten(io_lib:format(F, T)).

warning(P, X) ->
    io:format("~p> WARNING ~p~n", [P,fmt(X)]).

debug(P, X) ->
    io:format("~p> DEBUG ~p ~n", [P,fmt(X)]).


%%
%%
%%
parse_open(OpenMsg) ->
    ?BGP_OPEN_FMT = OpenMsg,
    debug("PEERCP", ["BGP Ident: ~p~n", _BGP_Identifier]),
    case _BGP_AS of
	65021 ->
	    {ok, _BGP_AS};
	_ ->
	    warning("PEERCP", ["Wrong ASN: ~p~n", _BGP_AS]),
	    {error, bad_as}
    end.


%%
%%
%%
parse_update_withdraw(<<>>) ->
    [];
parse_update_withdraw(Withdraw) ->
    %debug('PEERCP', ["blaha: ~p", Withdraw]),
    <<H:1, R1/binary>> = Withdraw,
    Hb = H*8,
    <<D:Hb, R2/binary>> = R1,
    [D|parse_update_withdraw(R2)].

%%
%%
%%
parse_int_array(_Size, <<>>) ->
    [];
parse_int_array(Size, Bin) ->
    <<D:Size, Rest/binary>> = Bin,
    [D|parse_int_array(Size, Rest)].

%%
%%
%%
parse_update_pathattr_as_path(Val) ->
    %%    debug('PEERCP', ["parse_update_pathattr_as_path: ~p", Val]),
    <<Type1:8, _Len:8, Rest/binary>> = Val,
    Type2 = case Type1 of
		   1 -> as_set;
		   2 -> as_sequence
	       end,
    {as_path, {Type2,
	       parse_int_array(16, Rest)}}.

%%
%%
%%
parse_update_pathattr_next_hop(Bin) ->
    <<A:8,B:8,C:8,D:8>> = Bin,
    {next_hop, (((256 * A + B) * 256 + C) * 256 + D)}.
%%
%%
%%
parse_update_pathattr_multi_exit_disc(Bin) ->
    <<MED:32>> = Bin,
    {multi_exit_disc, MED}.
%%
%%
%%
parse_update_pathattr_local_pref(Bin) ->
    <<L:32>> = Bin,
    {local_pref, L}.
%%
%%
%%
parse_update_pathattr_atomic_aggregate(_Bin) ->
    atomic_aggregate.
%%
%%
%%
parse_update_pathattr_aggregator(Bin) ->
    <<AS:16>> = Bin,
    {aggregator, AS}.

%%
%%
%%
parse_update_pathattr(<<>>) ->
    [];
parse_update_pathattr(Pathattr) ->
    debug('PEERCP', ["pathattr parse: ~p", Pathattr]),
    <<_F_Optional:1,
     _F_Transitive:1,
     _F_Partial:1,
     _F_Extended:1,
     _F_OtherFlags:4,
     Code:8,
     R1/binary>> = Pathattr,
    case _F_Extended of
	0 ->
	    <<Len:8, R2/binary>> = R1;
	1 ->
	    <<Len:16, R2/binary>> = R1
    end,
    <<Value:Len/binary, R3/binary>> = R2,
    Parsed = case Code of
		 ?BGP_PATHATTR_ORIGIN -> 
		     <<V:8>> = Value,
		     {origin, case V of
				  0 -> igp;
				  1 -> egp;
				  2 -> incomplete
			      end};
		 ?BGP_PATHATTR_AS_PATH ->
		     parse_update_pathattr_as_path(Value);
		 ?BGP_PATHATTR_NEXT_HOP -> 
		     parse_update_pathattr_next_hop(Value);
		 ?BGP_PATHATTR_MULTI_EXIT_DISC -> 
		     parse_update_pathattr_multi_exit_disc(Value);
		 ?BGP_PATHATTR_LOCAL_PREF -> 
		     parse_update_pathattr_local_pref(Value);
		 ?BGP_PATHATTR_ATOMIC_AGGREGATE -> 
		     parse_update_pathattr_atomic_aggregate(Value);
		 ?BGP_PATHATTR_AGGREGATOR -> 
		     parse_update_pathattr_aggregator(Value);
		 Any ->
		     warning('PEERCP', ["Unknown path attribute ~p", Any])
    end,
    [Parsed|parse_update_pathattr(R3)].


%%
%%
%%
parse_update_info(<<>>) ->
    [];
parse_update_info(Info) ->
    %debug('PEERCP', ["Update Info parse: ~p", Info]),
    <<Len:8, R1/binary>> = Info,
    GLen = (8-(Len rem 8)) rem 8,
    %debug('PEERCP', ["paaarse ~p ~p", Len, GLen]),
    <<Prefix1:Len,
     _Garbage:GLen,
     R2/binary>> = R1,
    Prefix2 = Prefix1 bsl (32-Len),
    [{Prefix2, Len}|parse_update_info(R2)].

%%
%%
%%
parse_update(Msg) ->
    ?BGP_UPDATE_FMT = Msg,

    %% Get withdraws
    ?BGP_UPDATE_FMT_WITHDRAW = _BGP_Update_Rest_Withdraw,

    %% Get path attributes
    ?BGP_UPDATE_FMT_PATHATTR = _BGP_Update_Rest_Pathattr,

    %% Parse the three field infos
    Withdraw = parse_update_withdraw(_BGP_Update_Withdraw),
    Pathattr = parse_update_pathattr(_BGP_Update_Pathattr),
    Info = parse_update_info(_BGP_Update_Info),

    {update, {withdraw, Withdraw,
	      pathattr, Pathattr,
	      info, Info}}.

%%
%%
%%
parse_all_msgs(_Peerp, <<>>) ->
    ok;

parse_all_msgs(Peerp, Msg) ->
    ?BGP_HEADER_FMT = Msg,
    <<Msg1:_BGP_Head_Length/binary, Msg2/binary>> = Msg,
    parse_msg(Peerp, Msg1),
    parse_all_msgs(Peerp, Msg2).

parse_msg(Peerp, Msg) ->
    ?BGP_HEADER_FMT = Msg,
    Peerp ! {self(), case _BGP_Head_Type of
			 ?BGP_TYPE_OPEN ->
			     {open, parse_open(Msg)};
			 ?BGP_TYPE_KEEPALIVE ->
			     keepalive;
			 ?BGP_TYPE_UPDATE ->
			     parse_update(Msg);
			 ?BGP_TYPE_NOTIFICATION ->
			     {notification, Msg};
			 Other ->
			     warning('PEERCP',
				     ["Unknown BGP message type: ~p",
				      Other]),
			     {error, Other}
		     end}.
    
%%
%%
%%
do_send_keepalive(Sock) ->
    %%debug("PEERCP", ["Sending keepalive"]),
    gen_tcp:send(Sock, ?BGP_KEEPALIVE_FMT),
    {ack, 'Sent keepalive'}.

do_send_open(Sock, _Peer) ->
    %%debug("PEERCP", ["Sending OPEN... ~p", Sock]),
    _BGP_Length = 29,
    _BGP_AS = 16#ffaf,
    _BGP_Holdtime = 180,
    _BGP_Identifier = 16#12345678,
    _BGP_Opt_Parm_Len = 0,
    _BGP_Unused = 0,
    _BGP_Rest = <<>>,
    Head = ?BGP_OPEN_FMT,
    gen_tcp:send(Sock, Head),
    {ack, 'Sent open'}.
    
do_announce_route(_Sock, _Path, []) ->
    {ack, announced};
do_announce_route(_Sock, Path, [R|Routes]) ->
    do_announce_route(_Sock, Path, R),
    do_announce_route(_Sock, Path, Routes);

do_announce_route(Sock, Path, Route) ->
    %%io:format("Route: ~p ~p~n", [Path, Route]),

    %% Info
    {Net,Plen} = Route,
    Tlen = Plen,
    %%io:format("~p ~p ~n", [Net, Plen]),
    TNet = Net bsr (32-Plen),
    _BGP_Update_Info = <<Plen:8,TNet:Tlen>>,
    %%io:format("Info: ~p~n", [_BGP_Update_Info]),

    %% Path attributes
    _BGP_Update_Pathattr = pathattr_factory(Path),
    _BGP_Update_Pathattrlen = size(_BGP_Update_Pathattr),
    %%io:format("Pathattr: ~p~n", [_BGP_Update_Pathattr]),
    _BGP_Update_Rest_Pathattr = ?BGP_UPDATE_FMT_PATHATTR,

    %% Withdraw
    _BGP_Update_Withdraw = <<>>,
    _BGP_Update_Withdrawlen = 0,
    _BGP_Update_Rest_Withdraw = ?BGP_UPDATE_FMT_WITHDRAW,

    %% Header
    _BGP_Length = 21 + 0
	+ 2 + _BGP_Update_Pathattrlen
	+ size(_BGP_Update_Info),
    Update = ?BGP_UPDATE_FMT,
    %%io:format("Packet: ~p~n", [Update]),
    gen_tcp:send(Sock, Update),
    {ack, {announce, Route}}.

array_to_binary(_Size, []) ->
    <<>>;
array_to_binary(Size, [H|T]) ->
    R = array_to_binary(Size, T),
    <<H:Size, R/binary>>.
pathattr_factory([]) ->
    <<>>;
pathattr_factory([{K,V}|Pathattrs]) ->
    Bin = case K of
	      origin ->
		  T = case V of
			  igp ->  0;
			  egp ->  1;
			  incomplete -> 2
		      end,
		  <<
		   ?BGP_PATHATTR_FLAG_TRANSITIVE:8,
		   ?BGP_PATHATTR_ORIGIN:8,
		   1:8,                                % len
		   T:8>>;
	      as_path ->
		  {ASType1, ASP} = V,
		  ASType2 = case ASType1 of
				as_sequence -> 
				    ?BGP_PATHATTR_AS_PATH_SEQUENTIAL;
				as_set -> 
				    ?BGP_PATHATTR_AS_PATH_SET
			    end,
		  L = length(ASP),
		  TotL = 2 + L * 2,
		  R = array_to_binary(16, ASP),
		  <<
		   ?BGP_PATHATTR_FLAG_TRANSITIVE:8,
		   ?BGP_PATHATTR_AS_PATH:8,
		   TotL:8,                             % attr len
		   ASType2:8,
		   L:8,                                % Path lenght
		   R/binary>>;
	      next_hop ->
		  <<
		   ?BGP_PATHATTR_FLAG_TRANSITIVE:8,
		   ?BGP_PATHATTR_NEXT_HOP:8,
		   4:8,                                % addrlen
		   V:32>>;
	      multi_exit_disc ->
		  <<
		   ?BGP_PATHATTR_FLAG_OPTIONAL:8,
		   ?BGP_PATHATTR_MULTI_EXIT_DISC:8,
		   4:8,                                % Val len
		   V:32>>
		      end,
    Rest = pathattr_factory(Pathattrs),
    <<Bin/binary, Rest/binary>>.
		  
%%
%% API
%%
connect(Peercp) ->
    Peercp ! {self(), connect}.
stop(Peercp) ->
    Peercp ! {self(), stop}.
send_open(Peercp) ->
    Peercp ! {self(), open}.
send_keepalive(Peercp) ->
    Peercp ! {self(), keepalive}.
announce_route(Peercp, Path, Route) ->
    Peercp ! {self(), {announce, Path, Route}}.

%%
%%
%%
state_idle(Peerp, Peer) ->
    receive
	{Peerp, connect} ->
	    {ok, Sock} = gen_tcp:connect(Peer#peer.ip,
					 Peer#peer.port,
					 [binary, {packet, 0}]),
	    Peerp ! {self(), {ok, connected}},
	    state_connected(Sock, Peerp, Peer)
    end.


state_connected(Sock, Peerp, Peer) ->
    receive
	%% From Peerp
	{Peerp, stop} ->
	    exit(killed);

	{Peerp, open} ->
	    Peerp ! {self(), do_send_open(Sock, Peer)},
	    state_connected(Sock, Peerp, Peer);

	{Peerp, keepalive} ->
	    Peerp ! {self(), do_send_keepalive(Sock)},
	    state_connected(Sock, Peerp, Peer);

	{Peerp, {announce, Path, Route}} ->
	    Peerp ! {self(), do_announce_route(Sock, Path, Route)},
	    state_connected(Sock, Peerp, Peer);

	{Peerp, {withdraw, _Route}} ->
	    Peerp ! {self(), {ack, withdraw}},
	    state_connected(Sock, Peerp, Peer);

	%% From socket
	{tcp, Sock, Msg} ->
	    parse_all_msgs(Peerp, Msg),
	    state_connected(Sock, Peerp, Peer);

	{tcp_closed, Sock} ->
	    Peerp ! {self(), {error, {tcp_closed, Sock}}},
	    gen_tcp:close(Sock),
	    state_idle(Peerp, Peer);
	    
	Any ->
	    io:format("state_connected got bogus msg ~p~n", [Any]),
	    state_connected(Sock, Peerp, Peer)
    end.

start(Peerp, Peer) ->
    spawn(?MODULE, state_idle, [Peerp, Peer]).
