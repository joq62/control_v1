%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(console).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

-define(SERVER,console_server).

-export([
	 new_cluster/1,
	 connect/0,
	 connect/1,

	 ping/0,
	 get_state/0
	]).

%debug
-export([
	 poll/0,
	 poll/1,
	 all_apps/0,
	 present_apps/0,
	 missing_apps/0,
	 call/5,
	 where_is_app/1
	]).

%% ====================================================================
%% External functions
%% ====================================================================

-export([
	 start/1,
	 stop/0
	]).




start([ClusterSpec])-> gen_server:start_link({local, ?SERVER}, ?SERVER, [ClusterSpec], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


	    
%% call
new_cluster(ClusterSpec)->
    gen_server:call(?SERVER, {new_cluster,ClusterSpec},infinity).
connect()->
    gen_server:call(?SERVER, {connect,glurk},infinity).

connect(ClusterSpec)->
    gen_server:call(?SERVER, {connect,ClusterSpec},infinity).

%% debug

all_apps()->
    gen_server:call(?SERVER, {all_apps},infinity).

present_apps() ->
    gen_server:call(?SERVER, {present_apps}).

missing_apps() ->
    gen_server:call(?SERVER, {missing_apps}).

where_is_app(App)->
    gen_server:call(?SERVER, {where_is_app,App},infinity).
call(PodNode,M,F,A,T)->
    gen_server:call(?SERVER, {call,PodNode,M,F,A,T},infinity).

ping() ->
    gen_server:call(?SERVER, {ping}).

get_state() ->
    gen_server:call(?SERVER, {get_state}).
%% cast

poll()->
    gen_server:cast(?SERVER, {poll}).
poll(TimeInMilliSecond)->
    gen_server:cast(?SERVER, {poll,TimeInMilliSecond}).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
