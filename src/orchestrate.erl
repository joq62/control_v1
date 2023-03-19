%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).

-define(SleepInterval,60*1000).
%% API
-export([
	 start/2,
	 start/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ClusterSpec,LeaderPid)->
%   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["LeaderPid",LeaderPid,node()]]),
    start(ClusterSpec,LeaderPid,?SleepInterval).

start(ClusterSpec,LeaderPid,SleepInterval)->
    timer:sleep(SleepInterval),
    Result=case leader:am_i_leader(LeaderPid,node(),5000) of
	       false->
		   [[],[],[],[],[]];
	       true->
		   orchistrate(ClusterSpec,SleepInterval)
	   end,
    rpc:cast(node(),control,orchestrate_result,Result).


orchistrate(ClusterSpec,SleepInterval)->
    timer:sleep(SleepInterval),

    ResultStartParents=rpc:call(node(),lib_control,start_parents,[],15*1000),

    ResultStartPods=rpc:call(node(),lib_control,start_pods,[],60*1000),

    ResultStartUserAppls=rpc:call(node(),lib_control,start_appls,[],60*1000), 
    ResultStartInfraAppls=[], %% Shall be removed

    [ResultStartParents,ResultStartPods,ResultStartInfraAppls,ResultStartUserAppls].
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
