%%%-------------------------------------------------------------------
%%% File    : records.hrl
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  26 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------

-record(peer, {ip,
	       port = 179,
	       as}).

-define(BGP_MARKER, 16#ffffffff).
-define(BGP_TYPE_OPEN, 1).
-define(BGP_TYPE_UPDATE, 2).
-define(BGP_TYPE_NOTIFICATION, 3).
-define(BGP_TYPE_KEEPALIVE, 4).
%% rfc2918 defines one more type


-define(BGP_HEADER_FMT, <<
			 ?BGP_MARKER:32,
			 ?BGP_MARKER:32,
			 ?BGP_MARKER:32,
			 ?BGP_MARKER:32,
			 _BGP_Head_Length:16,
			 _BGP_Head_Type:8,
			 _BGP_Head_rest/binary>>).

%% open
-define(BGP_VERSION, 4).
-define(BGP_OPEN_FMT, <<
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       _BGP_Length:16,
		       ?BGP_TYPE_OPEN:8,
		       ?BGP_VERSION:8,
		       _BGP_AS:16,
		       _BGP_Holdtime:16,
		       _BGP_Identifier:32,
		       _BGP_Opt_Parm_Len:8,
		       _BGP_Rest/binary>>).
%% followed by BGP_Opt_Parm_Len bytes

%% keepalive
-define(BGP_KEEPALIVE_LENGTH, 19).
-define(BGP_KEEPALIVE_FMT, <<
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       ?BGP_MARKER:32,
		       ?BGP_KEEPALIVE_LENGTH:16,
		       ?BGP_TYPE_KEEPALIVE:8>>).
