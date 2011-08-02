%%%-------------------------------------------------------------------
%%% File    : eggpd.app
%%% Author  : Thomas Habets <thomas@habets.pp.se>
%%% Description : 
%%%
%%% Created :  24 Jul 2008 by Thomas Habets <thomas@habets.pp.se>
%%%-------------------------------------------------------------------
{application, eggpd, 
 [{description, "EGG BGPD"},
  {vsn, "0.01"},
  {modules, [eggpd,
  	     eggpd_app,
	     eggpd_sup,
	     ribp, 
	     fibp,
	     peerp]},	
  {registered,[eggpd_super,
	       ribp,
	       fibp]},
  {applications, [kernel,stdlib]},
  {mod, {eggpd_app,[]}},
  {start_phases, []}
 ]}.
