%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrate).


-define(LogDir,"log_dir").
-define(LogFileName,"file.logs").
-define(SleepInterval,60*1000).
%% API
-export([
	 start/1,
	 start/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start(ClusterSpec)->
    start(ClusterSpec,?SleepInterval).

start(ClusterSpec,SleepInterval)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG orchistrate  : ",time(),?MODULE,?LINE]]),
    timer:sleep(SleepInterval),
%    ResultStartParents=debug1,
    ResultStartParents=rpc:call(node(),lib_control,start_parents,[],15*1000),
%    sd:cast(nodelog,nodelog,log,[notice,lib_control_STRING,?LINE,["ResultStartParents  : ",ResultStartParents,?MODULE,?LINE]]),
 %   ResultStartPods=debug2,
    ResultStartPods=rpc:call(node(),lib_control,start_pods,[],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartPods  : ",ResultStartPods,?MODULE,?LINE]]),
    
 %   ResultStartInfraAppls=debug3,
    ResultStartInfraAppls=rpc:call(node(),lib_control,start_infra_appls,[ClusterSpec],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartInfraAppls  : ",ResultStartInfraAppls,?MODULE,?LINE]]),
%    ResultStartUserAppls=debug4,
    ResultStartUserAppls=rpc:call(node(),lib_control,start_user_appls,[],60*1000), 
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartUserAppls  : ",ResultStartUserAppls,?MODULE,?LINE]]),


    rpc:cast(node(),infra_service,orchistrate_result,[ResultStartParents,
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
