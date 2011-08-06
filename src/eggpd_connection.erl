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
-export([state_idle/3,
	 state_connected/4]).

%% Internal
-include("records.hrl").

%%--------------------------------------------------------------------
%% Function: parse_open_parameter(Type, Len, Data) -> parm_description
%%
%% Description:
%% Create OPEN parameter descriptions from raw data.
%%
%% References:
%% RFC2842 - Capabilities
%% RFC2918 - Route refresh
%% RFC4893 - 32bit AS
%%--------------------------------------------------------------------
parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES,
		     <<?CAP_MP_EXT,
		       4,             % Len
		       0, 1,          % IPv4
		       0,             % Reserved
		       1              % Unicast
		     >>) ->
    bgp_option_multiprotocol_ipv4;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES,
		     <<?CAP_MP_EXT,
		       4,             % Len
		       0, 2,          % IPv6
		       0,             % Reserved
		       1              % Unicast
		     >>) ->
    bgp_option_multiprotocol_ipv6;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES,
		     <<?CAP_ROUTE_REFRESH,
		       0              % Len
		     >>) ->
    bgp_option_route_refresh;

%% TODO: what is cap 128 exactly? How is it different from 2?
parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES,
		     <<?CAP_ROUTE_REFRESH_128,
		       0              % Len
		     >>) ->
    bgp_option_route_refresh_128;

parse_open_parameter(?BGP_OPEN_PARM_CAPABILITIES,
		     <<?CAP_32BIT_AS,
		       4,             % Len
		       AS:32>>) ->
    {bgp_option_32bit_as, AS}.

%%--------------------------------------------------------------------
%% Function: parse_open_parameters(OPENData) -> [parm_description, ...]
%%
%% Description:
%% Given OPEN parameters as a binary return a list of parsed parameters.
%%--------------------------------------------------------------------
parse_open_parameters(Data) ->
    parse_open_parameters(Data, []).

parse_open_parameters(<<>>, Opts) ->
    Opts;

parse_open_parameters(Data, Opts) ->
    ?BGP_OPEN_PARM_FMT = Data,
    <<OptData:_BGP_Open_Parm_Len/binary, Rest/binary>> = _BGP_Open_Parm_Rest,
    Opt = parse_open_parameter(_BGP_Open_Parm_Type,
			       OptData),
    parse_open_parameters(Rest, [Opt | Opts]).

%%--------------------------------------------------------------------
%% Function: parse_open(OpenMsg, Peer) ->
%%             {as, AS,
%%              holdtime, Holdtime
%%              identifier, Identifier,
%%              parameters, ParameterList
%%             }
%%
%% Description:
%% Convert an OPEN message from raw binary to parsed structure.
%%--------------------------------------------------------------------
parse_open(OpenMsg, Peer) ->
    ?BGP_OPEN_FMT = OpenMsg,
    io:format("con(~p): BGP Ident: ~p~n", [self(), _BGP_Identifier]),
    _BGP_AS = Peer#peer.as,
    _BGP_Opt_Parm_Len = size(_BGP_Rest),
    {as, _BGP_AS,
     holdtime, _BGP_Holdtime,
     identifier, _BGP_Identifier,
     parameters, parse_open_parameters(_BGP_Rest)}.

%%--------------------------------------------------------------------
%% Function: parse_int_array(Size, Data) -> [n, m, ...]
%%
%% Description:
%% Extract a list of 'Size'-bit ints from 'Data'.
%%--------------------------------------------------------------------
parse_int_array(Size, Data) ->
    parse_int_array(Size, Data, []).

parse_int_array(_Size, <<>>, Arr) ->
    lists:reverse(Arr);

parse_int_array(Size, Bin, Arr) ->
    <<D:Size, Rest/binary>> = Bin,
    parse_int_array(Size, Rest, [D|Arr]).

%%--------------------------------------------------------------------
%% Function: parse_update_pathattr(Type, Binary) -> path_attr_spec
%%
%% Description:
%% Convert a path attribute binary into a parsed structure.
%%--------------------------------------------------------------------
parse_update_pathattr(?BGP_PATHATTR_AS_PATH,
		      <<?AS_SET,
			_Len:8,
			Rest/binary>>) ->
    {as_path, {as_set, parse_int_array(16, Rest)}};

parse_update_pathattr(?BGP_PATHATTR_AS_PATH,
		      <<?AS_SEQUENCE,
			_Len:8,
			Rest/binary>>) ->
    {as_path, {as_sequence, parse_int_array(16, Rest)}};

parse_update_pathattr(?BGP_PATHATTR_NEXT_HOP,
		      <<A:8, B:8, C:8, D:8>>) ->

    {next_hop, (((256 * A + B) * 256 + C) * 256 + D)};

parse_update_pathattr(?BGP_PATHATTR_MULTI_EXIT_DISC, <<MED:32>>) ->
    {multi_exit_disc, MED};

parse_update_pathattr(?BGP_PATHATTR_LOCAL_PREF, <<LocalPref:32>>) ->
    {local_pref, LocalPref};

parse_update_pathattr(?BGP_PATHATTR_ATOMIC_AGGREGATE, _Bin) ->
    atomic_aggregate;

parse_update_pathattr(?BGP_PATHATTR_AGGREGATOR, <<AS:16, IP:32>>) ->
    {aggregator, AS, IP};

parse_update_pathattr(?BGP_PATHATTR_MP_REACH_NLRI,
		      <<0,2,                % IPv6
			1,                  % Unicast
			16,                 % Nexthop length
			Nexthop:16/binary,  % Nexthop
			0,                  % Point of attachment
			PrefixLen,          % Prefixlen
			Net/binary          % Net. May be <PrefixLen bits
		      >>) ->
    %% FIXME: canonicalize the address size
    {nlri_ipv6, Net, PrefixLen, Nexthop};

parse_update_pathattr(?BGP_PATHATTR_ORIGIN, <<?ORIGIN_IGP>>) -> {origin, igp};
parse_update_pathattr(?BGP_PATHATTR_ORIGIN, <<?ORIGIN_EGP>>) -> {origin, egp};
parse_update_pathattr(?BGP_PATHATTR_ORIGIN, <<?ORIGIN_INCOMPLETE>>) ->
    {origin, incomplete};

%% Default catchall: Ignore attribute.
parse_update_pathattr(PathAttr, Value) ->
    io:format("Unknown path attribute ~p = ~p~n", [PathAttr, Value]),
    {unknown, PathAttr, Value}.

%%--------------------------------------------------------------------
%% Function: parse_update_pathattrs(Data) -> [path_attribute, ...]
%%
%% Description:
%% Given UPDATE path attributes as a binary return a list of parsed
%% structures..
%%--------------------------------------------------------------------
parse_update_pathattrs(Data) ->
    parse_update_pathattrs(Data, []).

parse_update_pathattrs(<<>>, Attrs) ->
    Attrs;

parse_update_pathattrs(<<_F_Optional:1,
			 _F_Transitive:1,
			 _F_Partial:1,
			 _F_Extended:1,
			 _F_OtherFlags:4,
			 Code:8,
			 R1/binary>>, Attrs) ->
    case _F_Extended of
	0 ->
	    <<Len:8, R2/binary>> = R1;
	1 ->
	    <<Len:16, R2/binary>> = R1
    end,
    <<Value:Len/binary, R3/binary>> = R2,
    Parsed = parse_update_pathattr(Code, Value),
    parse_update_pathattrs(R3, [Parsed|Attrs]).

%%--------------------------------------------------------------------
%% Function: parse_update_info(Binary) -> [network, ...]
%%
%% Description:
%% Parses a binary info or withdraw from an UPDATE message.
%%--------------------------------------------------------------------
parse_update_info(Bin) ->
    parse_update_info(Bin, []).

parse_update_info(<<>>, Info) ->
    Info;

parse_update_info(<<Len:8, R1/binary>>, Info) ->
    GLen = (8-(Len rem 8)) rem 8,
    <<Prefix1:Len,
      _Garbage:GLen,
      R2/binary>> = R1,
    Prefix2 = Prefix1 bsl (32-Len),
    parse_update_info(R2, [{Prefix2, Len}|Info]).

%%--------------------------------------------------------------------
%% Function: parse_update(Data) -> {update,
%%                                     {withdraw, Withdraw,
%%                                      pathattr, Pathattr,
%%                                      info, Info}}
%%
%% Description:
%% Parse an UPDATE message.
%%--------------------------------------------------------------------
parse_update(Msg) ->
    ?BGP_UPDATE_FMT = Msg,
    ?BGP_UPDATE_FMT_WITHDRAW = _BGP_Update_Rest_Withdraw,
    ?BGP_UPDATE_FMT_PATHATTR = _BGP_Update_Rest_Pathattr,

    Withdraw = parse_update_info(_BGP_Update_Withdraw),
    Pathattr = parse_update_pathattrs(_BGP_Update_Pathattr),
    Info = parse_update_info(_BGP_Update_Info),

    {update, {withdraw, Withdraw,
	      pathattr, Pathattr,
	      info, Info}}.

%%--------------------------------------------------------------------
%% Function: parse_all_msgs(Prent, Data, Peer) -> ignored
%%
%% Description:
%% Extract all complete messages from the buffer and send them off for
%% parsing & handling. Leave any half-received data in the buffer.
%%--------------------------------------------------------------------
parse_all_msgs(_Parent, <<>>, _Peer) ->
    void;

parse_all_msgs(_Parent, Msg, _Peer) when size(Msg) < 19 ->
    put(tcp_buffer, Msg);

parse_all_msgs(Parent, Msg, Peer) ->
    ?BGP_HEADER_FMT = Msg,
    if
	size(Msg) < _BGP_Head_Length ->
	    put(tcp_buffer, Msg);
		
	true ->
	    <<Msg1:_BGP_Head_Length/binary, Msg2/binary>> = Msg,
	    parse_msg(Parent, _BGP_Head_Type, Msg1, Peer),
	    parse_all_msgs(Parent, Msg2, Peer)
    end.

%%--------------------------------------------------------------------
%% Function: parse_msg(Parent, Msg, Peer) -> ok
%%
%% Description:
%% Parse BGP messages and generate calls to Peer process.
%%--------------------------------------------------------------------
parse_msg(Parent, ?BGP_TYPE_OPEN, Msg, Peer) ->
    gen_fsm:send_event(Parent, {open, parse_open(Msg, Peer)});

parse_msg(Parent, ?BGP_TYPE_KEEPALIVE, _Msg, _Peer) ->
    gen_fsm:send_event(Parent, keepalive);

parse_msg(Parent, ?BGP_TYPE_UPDATE, Msg, _Peer) ->
    gen_fsm:send_event(Parent, parse_update(Msg));

parse_msg(Parent, ?BGP_TYPE_NOTIFICATION, Msg, _Peer) ->
    gen_fsm:send_event(Parent, {notification, Msg}).

%%--------------------------------------------------------------------
%% Function: do_send_keepalive(Sock) -> ignored
%%
%% Description:
%% Send simple KEEPALIVE.
%%--------------------------------------------------------------------
do_send_keepalive(Sock) ->
    gen_tcp:send(Sock, ?BGP_KEEPALIVE_FMT).

%%--------------------------------------------------------------------
%% Function: do_send_open(Sock, peer) -> ignored
%%
%% Description:
%% Generate and send OPEN.
%%--------------------------------------------------------------------
do_send_open(Sock, Peer) ->
    _BGP_Length = 29,
    _BGP_AS = Peer#peer.localas,
    _BGP_Holdtime = 180,
    _BGP_Identifier = 16#12345678,
    _BGP_Opt_Parm_Len = 0,
    _BGP_Unused = 0,
    _BGP_Rest = <<>>,
    gen_tcp:send(Sock, ?BGP_OPEN_FMT).

    
%%--------------------------------------------------------------------
%% Function: do_announce_route(Sock, Path, Nets) -> ignored
%%   Path: List of ASs
%%   Nets
%% Description:
%%--------------------------------------------------------------------
do_announce_route(_Sock, _Path, []) ->
    void;

%% TODO: this function only works for /32s, or possibly /8, /16 and /24 too
do_announce_route(Sock, Path, [OneRoute|Routes]) ->
    %% Info
    {Net,Plen} = OneRoute,
    Tlen = Plen,
    TNet = Net bsr (32-Plen),
    _BGP_Update_Info = <<Plen:8,TNet:Tlen>>,

    %% Path attributes
    _BGP_Update_Pathattr = pathattr_factory(Path),
    _BGP_Update_Pathattrlen = size(_BGP_Update_Pathattr),
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
    gen_tcp:send(Sock, Update),
    do_announce_route(Sock, Path, Routes).


%%--------------------------------------------------------------------
%% Function: array_to_binary(Size, Array) -> binary
%%
%% Description:
%%--------------------------------------------------------------------
array_to_binary(_Size, []) ->
    <<>>;

array_to_binary(Size, [H|T]) ->
    R = array_to_binary(Size, T),
    <<H:Size, R/binary>>.

%%--------------------------------------------------------------------
%% Function: pathattr_factory(Pathattrs) -> binary
%%
%% Description:
%% Convert pathattr structures to binary.
%%--------------------------------------------------------------------
pathattr_factory(Attrs) ->
    list_to_binary(lists:map(fun pathattr_factory_entry/1, Attrs)).

pathattr_factory_entry({origin, V}) ->
    Origin = case V of
		 igp ->  ?ORIGIN_IGP;
		 egp ->  ?ORIGIN_EGP;
		 incomplete -> ?ORIGIN_INCOMPLETE
	     end,
    <<?BGP_PATHATTR_FLAG_TRANSITIVE:8,
      ?BGP_PATHATTR_ORIGIN:8,
      1:8,                                % len
      Origin:8>>;

pathattr_factory_entry({as_path, {ASType1, ASP}}) ->
    ASType2 = case ASType1 of
		  as_sequence ->
		      ?BGP_PATHATTR_AS_PATH_SEQUENTIAL;
		  as_set ->
		      ?BGP_PATHATTR_AS_PATH_SET
	      end,
    L = length(ASP),
    TotL = 2 + L * 2,
    R = array_to_binary(16, ASP),
    <<?BGP_PATHATTR_FLAG_TRANSITIVE:8,
      ?BGP_PATHATTR_AS_PATH:8,
      TotL:8,                             % attr len
      ASType2:8,
      L:8,                                % Path lenght
      R/binary>>;

pathattr_factory_entry({next_hop, V}) ->
    <<?BGP_PATHATTR_FLAG_TRANSITIVE:8,
      ?BGP_PATHATTR_NEXT_HOP:8,
      4:8,                                % addrlen
      V:32>>;

pathattr_factory_entry({multi_exit_disc, V}) ->
    <<?BGP_PATHATTR_FLAG_OPTIONAL:8,
      ?BGP_PATHATTR_MULTI_EXIT_DISC:8,
      4:8,                                % Val len
      V:32>>.
		  
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
%% Ugly state and message loop below. Should be rewritten with generic
%% behaviour.
%%
state_idle(Parent, Peer) ->
    io:format("con(~p): state_idle/2~n", [self()]),
    enter_state_idle(false, Parent, Peer).

enter_state_idle(LSock, Parent, Peer) ->
    io:format("con(~p): entering idle~n", [self()]),
    io:format("con(~p): state_idle~n", [self()]),
    state_idle(LSock, Parent, Peer).

state_idle(LSock, Parent, Peer) ->
    io:format("con(~p): idle(~p, ~p, ~p)~n", [self(), LSock, Parent, Peer]),
    receive
	{Parent, stop} ->
	    exit(killed);

	{Parent, connect} ->
	    R = gen_tcp:connect(Peer#peer.ip,
				Peer#peer.port,
				[binary, {packet, 0}],
				1000),
	    case R of
		{ok, Sock} ->
		    enter_state_connected(LSock, Sock, Parent, Peer);
		Any ->
		    io:format("con(~p): connect() failed: ~p~n",
			      [self(), Any]),
		    Parent ! {self(), {error, {connect, timeout}}},
		    ?MODULE:state_idle(LSock, Parent, Peer)
	    end;

	{Parent, listen} ->
	    LS = gen_tcp:listen(Peer#peer.localport,
				[binary]),
	    case LS of
		{ok, LS1} ->
		    ?MODULE:state_idle(LS1, Parent, Peer);
		Any ->
		    io:format("con(~p): listen() failed: ~p~n", [self(), Any]),
		    state_idle(LSock, Parent, Peer)
	    end
    after 1000 ->
	    case LSock of
		false ->
		    ?MODULE:state_idle(LSock, Parent, Peer);
		_ ->
		    R = gen_tcp:accept(LSock, 0),
		    case R of
			{ok, Sock} ->
			    enter_state_connected(LSock, Sock, Parent, Peer);
			_ ->
			    ?MODULE:state_idle(LSock, Parent, Peer)
		    end
	    end
    end.


enter_state_connected(LSock, Sock, Parent, Peer) ->
    io:format("con(~p): state_connected~n", [self()]),
    gen_fsm:send_event(Parent, connected),
    put(tcp_buffer, <<>>),
    state_connected(LSock, Sock, Parent, Peer).

state_connected(LSock, Sock, Parent, Peer) ->
    receive
	%% From Parent
	{Parent, stop} ->
	    exit(killed);

	{Parent, open} ->
	    io:format("con(~p): Sending open~n", [self()]),
	    do_send_open(Sock, Peer),
	    ?MODULE:state_connected(LSock, Sock, Parent, Peer);

	{Parent, keepalive} ->
	    io:format("con(~p): Sending keepalive~n", [self()]),
	    do_send_keepalive(Sock),
	    ?MODULE:state_connected(LSock, Sock, Parent, Peer);

	{Parent, {announce, Path, Route}} ->
	    io:format("con(~p): announce ~p ~p~n", [self(), Path, Route]),
	    do_announce_route(Sock, Path, Route),
	    ?MODULE:state_connected(LSock, Sock, Parent, Peer);

	{Parent, {withdraw, _Route}} ->
	    %% FIXME
	    ?MODULE:state_connected(LSock, Sock, Parent, Peer);

	%% From socket
	{tcp, Sock, Msg} ->
	    Msg1 = list_to_binary([get(tcp_buffer), Msg]),
	    put(tcp_buffer, <<>>),
	    parse_all_msgs(Parent, Msg1, Peer),
	    ?MODULE:state_connected(LSock, Sock, Parent, Peer);

	{tcp_closed, Sock} ->
	    Parent ! {self(), {error, {tcp_closed, Sock}}},
	    gen_tcp:close(Sock),
	    ?MODULE:state_idle(LSock, Parent, Peer)
    end.

start_link(Peer) ->
    io:format("con(new): ~p spawning ~p...~n", [self(), Peer]),
    spawn_link(?MODULE, state_idle, [self(), Peer]).
