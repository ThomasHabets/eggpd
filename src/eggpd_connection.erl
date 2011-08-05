%%%-------------------------------------------------------------------
%%% File    : eggpd_connection.erl
%%% Author  : Thomas Habets <thomas@habets.se>
%%% Description :
%%%   Impure module.
%%%   Handles TCP connection and parses BGP messages and turns them into
%%%   gen_fsm:send_event/2 into eggpd_peer.
%%%
%%%   TODO: Rewrite as gen_fsm or gen_server.
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
-module(eggpd_connection).

%% Functions called by startup/tairdown
-export([start_link/1, stop/1, state_idle/2]).

%% API
-export([announce_route/3,
	 connect/1,
	 listen/1,
	 send_keepalive/1,
	 send_open/1]).

%% States
-export([state_idle/3]).

%% Internal
-include("records.hrl").


parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES, _, <<1,4,0,1,0,1>>) ->
    bgp_option_multiprotocol_ipv4;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES, _, <<1,4,0,2,0,1>>) ->
    bgp_option_multiprotocol_ipv6;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES, _, <<128,0>>) ->
    bgp_option_route_refresh_128;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES, _, <<2,0>>) ->
    bgp_option_route_refresh_2;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES, _, <<65,4,AS:32>>) ->
    {bgp_option_32bit_as, AS}.

parse_open_parameters(Data) ->
    parse_open_parameters(Data, []).

parse_open_parameters(<<>>, Opts) ->
    Opts;
parse_open_parameters(Data, Opts) ->
    ?BGP_OPEN_PARM_FMT = Data,
    <<OptData:_BGP_Open_Parm_Len/binary, Rest/binary>> = _BGP_Open_Parm_Rest,
    Opt = parse_open_parameter(_BGP_Open_Parm_Type,
			       _BGP_Open_Parm_Len,
			       OptData),
    parse_open_parameters(Rest, [Opt | Opts]).

%%
%%
%%
parse_open(OpenMsg, Peer) ->
    ?BGP_OPEN_FMT = OpenMsg,
    io:format("con(~p): BGP Ident: ~p~n", [self(), _BGP_Identifier]),
    _BGP_AS = Peer#peer.as,
    %% TODO: check that _BGP_Opt_Parm_Len is correct
    {as, _BGP_AS,
     holdtime, _BGP_Holdtime,
     identifier, _BGP_Identifier,
     parameters, parse_open_parameters(_BGP_Rest)}.

%%
%%
%%
parse_update_withdraw(<<>>) ->
    [];
parse_update_withdraw(Withdraw) ->
    %io:format('EGGPD_CONNECTION', ["blaha: ~p", Withdraw]),
    <<Len:8, R1/binary>> = Withdraw,
    GLen = (8-(Len rem 8)) rem 8,
    %io:format('EGGPD_CONNECTION', ["paaarse ~p ~p", Len, GLen]),
    <<Prefix1:Len,
     _Garbage:GLen,
     R2/binary>> = R1,
    Prefix2 = Prefix1 bsl (32-Len),
    [{Prefix2, Len}|parse_update_info(R2)].

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
    %%    io:format('EGGPD_CONNECTION', ["parse_update_pathattr_as_path: ~p", Val]),
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
    <<AS:16, IP:32>> = Bin,
    {aggregator, AS, IP}.

%%
%%
%%
parse_update_pathattrs(<<>>) ->
    [];
parse_update_pathattrs(Pathattr) ->
    %%io:format('EGGPD_CONNECTION', ["pathattr parse: ~p", Pathattr]),
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
		     io:format("Unknown path attribute ~p~n", [Any])
    end,
    [Parsed|parse_update_pathattrs(R3)].


%%
%%
%%
parse_update_info(<<>>) ->
    [];
parse_update_info(Info) ->
    %io:format('EGGPD_CONNECTION', ["Update Info parse: ~p", Info]),
    <<Len:8, R1/binary>> = Info,
    GLen = (8-(Len rem 8)) rem 8,
    %io:format('EGGPD_CONNECTION', ["paaarse ~p ~p", Len, GLen]),
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
    Pathattr = parse_update_pathattrs(_BGP_Update_Pathattr),
    Info = parse_update_info(_BGP_Update_Info),

    {update, {withdraw, Withdraw,
	      pathattr, Pathattr,
	      info, Info}}.

%%
%%
%%
parse_all_msgs(_Peerp, <<>>, _Peer) ->
    ok;

parse_all_msgs(Peerp, Msg, Peer) ->
    if
	size(Msg) < 19 ->
	    put(tcp_buffer, Msg);
	true ->
	    ?BGP_HEADER_FMT = Msg,
	    if
		size(Msg) < _BGP_Head_Length ->
		    put(tcp_buffer, Msg);
		
		size(Msg) == _BGP_Head_Length ->
		    parse_msg(Peerp, Msg, Peer);
		
		size(Msg) > _BGP_Head_Length ->
		    <<Msg1:_BGP_Head_Length/binary, Msg2/binary>> = Msg,
		    parse_msg(Peerp, Msg1, Peer),
		    parse_all_msgs(Peerp, Msg2, Peer)
	    end
    end.

parse_msg(Parent, Msg, Peer) ->
    ?BGP_HEADER_FMT = Msg,
    io:format("con(~p): about to send event to ~p~n", [self(), Parent]),
    gen_fsm:send_event(Parent,
		       case _BGP_Head_Type of
			   ?BGP_TYPE_OPEN ->
			       {open, parse_open(Msg, Peer)};
			   ?BGP_TYPE_KEEPALIVE ->
			       keepalive;
			   ?BGP_TYPE_UPDATE ->
			       parse_update(Msg);
			   ?BGP_TYPE_NOTIFICATION ->
			       {notification, Msg};
			   Other ->
			       io:format("con(~p): Unknown BGP msg type: ~p",
					 [self(), Other]),
			       {error, Other}
		       end).

%%
%%
%%
do_send_keepalive(Sock) ->
    %%io:format("EGGPD_CONNECTION", ["Sending keepalive"]),
    gen_tcp:send(Sock, ?BGP_KEEPALIVE_FMT),
    {ack, 'Sent keepalive'}.

do_send_open(Sock, Peer) ->
    %%io:format("EGGPD_CONNECTION", ["Sending OPEN... ~p", Sock]),
    _BGP_Length = 29,
    _BGP_AS = Peer#peer.localas,
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
connect(Con) ->
    Con ! {self(), connect}.
listen(Con) ->
    Con ! {self(), listen}.
stop(Con) ->
    Con ! {self(), stop}.
send_open(Con) ->
    Con ! {self(), open}.
send_keepalive(Con) ->
    Con ! {self(), keepalive}.
announce_route(Con, Path, Route) ->
    Con ! {self(), {announce, Path, Route}}.

%%
%%
%%
state_idle(Parent, Peer) ->
    io:format("con(~p): state_idle/2~n", [self()]),
    enter_state_idle(false, Parent, Peer).

enter_state_idle(LSock, Peerp, Peer) ->
    io:format("con(~p): entering idle~n", [self()]),
    io:format("con(~p): state_idle~n", [self()]),
    state_idle(LSock, Peerp, Peer).

state_idle(LSock, Peerp, Peer) ->
    io:format("con(~p): idle(~p, ~p, ~p)~n", [self(), LSock, Peerp, Peer]),
    receive
	{Peerp, stop} ->
	    exit(killed);

	{Peerp, connect} ->
	    R = gen_tcp:connect(Peer#peer.ip,
				Peer#peer.port,
				[binary, {packet, 0}],
				1000),
	    case R of
		{ok, Sock} ->
		    enter_state_connected(LSock, Sock, Peerp, Peer);
		Any ->
		    io:format("con(~p): connect() failed: ~p~n",
			      [self(), Any]),
		    Peerp ! {self(), {error, {connect, timeout}}},
		    state_idle(LSock, Peerp, Peer)
	    end;

	{Peerp, listen} ->
	    LS = gen_tcp:listen(Peer#peer.localport,
				[binary]),
	    case LS of
		{ok, LS1} ->
		    state_idle(LS1, Peerp, Peer);
		Any ->
		    io:format("con(~p): listen() failed: ~p~n", [self(), Any]),
		    state_idle(LSock, Peerp, Peer)
	    end
    after 1000 ->
	    case LSock of
		false ->
		    state_idle(LSock, Peerp, Peer);
		_ ->
		    R = gen_tcp:accept(LSock, 0),
		    case R of
			{ok, Sock} ->
			    enter_state_connected(LSock, Sock, Peerp, Peer);
			_ ->
			    ?MODULE:state_idle(LSock, Peerp, Peer)
		    end
	    end
    end.


enter_state_connected(LSock, Sock, Parent, Peer) ->
    io:format("con(~p): state_connected~n", [self()]),
    gen_fsm:send_event(Parent, connected),
    put(tcp_buffer, <<>>),
    state_connected(LSock, Sock, Parent, Peer).

state_connected(LSock, Sock, Peerp, Peer) ->
    receive
	%% From Peerp
	{Peerp, stop} ->
	    exit(killed);

	{Peerp, open} ->
	    io:format("con(~p): Sending open~n", [self()]),
	    do_send_open(Sock, Peer),
	    state_connected(LSock, Sock, Peerp, Peer);

	{Peerp, keepalive} ->
	    io:format("con(~p): Sending keepalive~n", [self()]),
	    do_send_keepalive(Sock),
	    state_connected(LSock, Sock, Peerp, Peer);

	{Peerp, {announce, Path, Route}} ->
	    do_announce_route(Sock, Path, Route),
	    state_connected(LSock, Sock, Peerp, Peer);

	{Peerp, {withdraw, _Route}} ->
	    %% FIXME
	    state_connected(LSock, Sock, Peerp, Peer);

	%% From socket
	{tcp, Sock, Msg} ->
	    Msg1 = list_to_binary([get(tcp_buffer), Msg]),
	    put(tcp_buffer, <<>>),
	    parse_all_msgs(Peerp, Msg1, Peer),
	    state_connected(LSock, Sock, Peerp, Peer);

	{tcp_closed, Sock} ->
	    Peerp ! {self(), {error, {tcp_closed, Sock}}},
	    gen_tcp:close(Sock),
	    state_idle(LSock, Peerp, Peer);
	    
	Any ->
	    io:format("con(~p): state_connected got bogus msg ~p~n",
		      [self(), Any]),
	    state_connected(LSock, Sock, Peerp, Peer)
    end.

start_link(Peer) ->
    io:format("con(new): ~p spawning ~p...~n", [self(), Peer]),
    spawn_link(?MODULE, state_idle, [self(), Peer]).
