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
		   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["am_i_leader",false,node()]]),
		   [[],[],[],[],[]];
	       true->
	%	   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["am_i_leader",true,node()]]),
		   orchistrate(ClusterSpec,SleepInterval)
	   end,
    rpc:cast(node(),control,orchestrate_result,Result).


orchistrate(ClusterSpec,SleepInterval)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["IsLeader",true,node()]]),
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG orchistrate  : ",time(),?MODULE,?LINE]]),
    timer:sleep(SleepInterval),
%    ResultStartParents=debug1,
    ResultStartParents=rpc:call(node(),lib_control,start_parents,[],15*1000),
 %   sd:cast(nodelog,nodelog,log,[notice,lib_control_STRING,?LINE,["ResultStartParents  : ",ResultStartParents,?MODULE,?LINE]]),
 %   ResultStartPods=debug2,
    ResultStartPods=rpc:call(node(),lib_control,start_pods,[],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartPods  : ",ResultStartPods,?MODULE,?LINE]]),
    
 %   ResultStartInfraAppls=debug3,
    ResultStartInfraAppls=rpc:call(node(),lib_control,start_infra_appls,[ClusterSpec],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartInfraAppls  : ",ResultStartInfraAppls,?MODULE,?LINE]]),
%    ResultStartUserAppls=debug4,
    ResultStartUserAppls=rpc:call(node(),lib_control,start_user_appls,[],60*1000), 
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartUserAppls  : ",ResultStartUserAppls,?MODULE,?LINE]]),

  %  timer:sleep(3000), %% If there is an election started
    [ResultStartParents,ResultStartPods,ResultStartInfraAppls,ResultStartUserAppls].
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
