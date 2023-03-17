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
start(ClusterSpec,false)->
   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["IsLEader",false,node()]]),
    start(ClusterSpec,false,?SleepInterval);

start(ClusterSpec,true)->
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["start_orchistrate",time(),node()]]),
    start(ClusterSpec,true,?SleepInterval).

start(_ClusterSpec,false,SleepInterval)->
    timer:sleep(SleepInterval),
    rpc:cast(node(),control,orchestrate_result,[[],
						[],
						[],
						[]]);

start(ClusterSpec,true,SleepInterval)->
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


    rpc:cast(node(),control,orchestrate_result,[ResultStartParents,
						      ResultStartPods,
						      ResultStartInfraAppls,
						      ResultStartUserAppls]).
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
