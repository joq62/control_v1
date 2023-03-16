%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 15 Mar 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(control).

-define(SERVER,control_server).

%% API
-export([
	 start_orchistrate/0,
	 orchestrate_result/4,
	 ping/0

	]).

-export([
	 start/1,
	 stop/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ClusterSpec)-> gen_server:start_link({local, ?SERVER}, ?SERVER, [ClusterSpec], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


ping() ->
    gen_server:call(?SERVER, {ping},infinity).

is_config()->
    gen_server:call(?SERVER, {is_config},infinity).
config(ClusterSpec)->
    gen_server:call(?SERVER, {config,ClusterSpec},infinity).
    
start_orchistrate()->
    gen_server:call(?SERVER, {start_orchistrate},infinity).
orchestrate_result(ResultStartParents,ResultStartPods,ResultStartInfraAppls,ResultStartUserAppls)->
    gen_server:cast(?SERVER,{orchestrate_result,
			     ResultStartParents,
			     ResultStartPods,
			     ResultStartInfraAppls,
			     ResultStartUserAppls}).  

%%%===================================================================
%%% Internal functions
%%%===================================================================

