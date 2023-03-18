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


%% Leader API
-export([
	 start_election/0,declare_victory/1,i_am_alive/1,
	 who_is_leader/0,am_i_leader/1,
	 ping_leader/0
	]).

-export([
	 start/0,
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
start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
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


%% API for leader 
who_is_leader()-> gen_server:call(?SERVER,{who_is_leader},infinity).
am_i_leader(Node)-> gen_server:call(?SERVER,{am_i_leader,Node},infinity).
ping_leader()-> gen_server:call(?SERVER,{ping_leader},infinity).    

%% election callbacks
start_election()-> gen_server:cast(?SERVER,{start_election}).
declare_victory(LeaderNode)-> gen_server:cast(?SERVER,{declare_victory,LeaderNode}).
i_am_alive(MyNode)-> gen_server:cast(?SERVER,{i_am_alive,MyNode}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

