%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(control_test).      
 
-export([start/1]).

-compile(export_all).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(LogDir,"log_dir").
-define(LogFileName,"file.logs").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec,_Arg2])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=lib_install:start_local_appls(ClusterSpec),
    true=lib_control:ensure_right_cookie(ClusterSpec),
    ok=lib_install:stop_all_pods(ClusterSpec),
    io:format("local apps running OK !!! ~p~n",[{?MODULE,?LINE,?FUNCTION_NAME}]),
    
    ok=lib_install:start_parents(ClusterSpec),
    io:format("ParentNode OK !!! ~p~n",[{?MODULE,?LINE,?FUNCTION_NAME}]),

    {ok,LogNode}=lib_install:start_log(ClusterSpec),
    io:format("Log started OK !!! ~p~n",[{LogNode,rpc:call(LogNode,erlang,nodes,[],5000),?MODULE,?LINE,?FUNCTION_NAME}]),

    {ok,EtcdNode}=lib_install:start_etcd(ClusterSpec),
    io:format("EtcdNode started OK !!! ~p~n",[{EtcdNode,rpc:call(EtcdNode,erlang,nodes,[],5000),?MODULE,?LINE,?FUNCTION_NAME}]),

    {ok,ControlNode}=lib_install:start_control(ClusterSpec),
    io:format("ControlNode started OK !!! ~p~n",[{ControlNode,rpc:call(ControlNode,erlang,nodes,[],5000),?MODULE,?LINE,?FUNCTION_NAME}]),
   
    WhichApplications=[{Node,rpc:call(Node,application,which_applications,[],5000)}||Node<-[LogNode|rpc:call(LogNode,erlang,nodes,[],5000)]],
    io:format("WhichApplications ~p~n",[{lists:sort(WhichApplications),?MODULE,?LINE,?FUNCTION_NAME}]),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    AllPods=db_pod_desired_state:pods(ClusterSpec),
    application:stop(etcd),
    [EtcdNode]=sd:get_node(etcd),

    io:format(" ****************************************************** ~n"),
    io:format("~p~n ",[{date(),time()}]),
    loop(ClusterSpec,AllPods,[]),
    
    

  %  init:stop(),
  %  timer:sleep(2000),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


loop1(ClusterSpec,AllNodes,PreviousNotice)->
    [AvailableNode|_]=[N2||N1<-AllNodes,
			    N2<-AllNodes,
			   pong==rpc:call(N1,net_adm,ping,[N2],5000),
			   pong==rpc:call(N2,sd,ping,[],5000),
			   N1/=N2],
    io:format("AvailableNode ~p~n ",[AvailableNode]),
    Notice=sd:call(log,log,all_parsed,[debug],5000),
    io:format(" ****************************************************** ~n"),
    io:format("~p~n ",[{date(),time()}]),
    io:format("~p~n ",[Notice]),
    NewPreviousNotice=Notice,
 %   Nodes=[AvailableNode|rpc:call(AvailableNode,erlang,nodes,[],6000)],
 %   io:format("Nodes ~p~n ",[Nodes]),
    timer:sleep(5*1000),
    loop1(ClusterSpec,AllNodes,NewPreviousNotice).
    %%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
loop(ClusterSpec,AllNodes,PreviousNotice)->
    [AvailableNode|_]=[N2||N1<-AllNodes,
			    N2<-AllNodes,
			   pong==rpc:call(N1,net_adm,ping,[N2],5000),
			   pong==rpc:call(N2,sd,ping,[],5000),
			   N1/=N2],
 %   io:format("AvailableNode ~p~n ",[AvailableNode]),
    Notice=sd:call(log,log,all_parsed,[debug],5000),
    NewPreviousNotice=case Notice==PreviousNotice of
			  true->
			      PreviousNotice;
			  false->
			      print(Notice,PreviousNotice),
			      Notice
		      end,
  %  Nodes=[AvailableNode|rpc:call(AvailableNode,erlang,nodes,[],6000)],
  %  io:format("Nodes ~p~n ",[Nodes]),
    timer:sleep(1*1000),
    loop(ClusterSpec,AllNodes,NewPreviousNotice).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
print(Notice,PreviousNotice)->
%    io:format(" ****************************************************** ~n"),
 %   io:format("~p~n ",[{date(),time()}]),
    NewItems=[Item||Item<-Notice,
		    false==lists:member(Item,PreviousNotice)],
    io:format(" ~p~n",[NewItems]),
  %  io:format("~n "),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_etcd(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),   
    Stopped=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    etcd==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(Stopped,[])],
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_nodelog(ClusterSpec,Parents)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %%-- nodelog -------------------------------------------------------------

    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),   
    StoppedNodelog=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    nodelog==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedNodelog,[])],
    
    {ok,ActiveApplsInfoList_2}=appl_server:active_appls(),
    [{NodelogNode,NodelogApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList_2,
					    nodelog==App],

    [rpc:call(Parent,net_adm,ping,[Pod],5000)||Parent<-Parents,
					       Pod<-[NodelogNode]],
    false=rpc:call(NodelogNode,nodelog,is_config,[],5000),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,NodelogNode),
    PathLogDir=filename:join(PodDir,?LogDir),
    rpc:call(NodelogNode,file,del_dir_r,[PathLogDir],5000),
    ok=rpc:call(NodelogNode,file,make_dir,[PathLogDir],5000),
    PathLogFile=filename:join([PathLogDir,?LogFileName]),
    ok=rpc:call(NodelogNode,nodelog,config,[PathLogFile],5000),
    true=rpc:call(NodelogNode,nodelog,is_config,[],5000),
    true=lists:keymember(nodelog,3,ActiveApplsInfoList_2),

    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_common_sd(ActivePods)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    CommonList=[{Pod,"common",common}||Pod<-ActivePods],
    []=[{R,Pod,ApplSpec,App}||{R,Pod,ApplSpec,App}<-create_appl(CommonList),
	   ok/=R],
    SdList=[{Pod,"sd",sd}||Pod<-ActivePods],
    []=[{R,Pod,ApplSpec,App}||{R,Pod,ApplSpec,App}<-create_appl(SdList),
	   ok/=R],
    
   
    ok.
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_local_appls(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=application:start(common),
    ok=application:start(sd),
    ok=application:start(etcd),
    pong=etcd:ping(),

    {
     "c200_c201","cookie_c200_c201","c200_c201",
     [{6,"c200"},{6,"c201"}]
    }=db_cluster_spec:read(ClusterSpec),
    
    ok=application:set_env([{control,[{cluster_spec,ClusterSpec}]}]),
    ok=application:start(control),
 %   {ok,_}=parent_server:start(),
    pong=parent_server:ping(),
    %--
 %   {ok,_}=pod_server:start(),
    pong=pod_server:ping(),
    %--
 %   {ok,_}=appl_server:start(),
    pong=appl_server:ping(),

    ok.




%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_pods(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    StartPods=[{pod_server:create_node(Pod),Pod}||Pod<-StoppedPods],
    {ok,ActivePods}=pod_server:active_nodes(),
    [{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActivePods,
								   Pod2<-ActivePods,
								   Pod1/=Pod2],
    {ok,ActivePods}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_parents(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    StartParents=[{parent_server:create_node(Parent),Parent}||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    [{rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
						Pod2<-ActiveParents,
						Pod1/=Pod2],
    {ok,ActiveParents}.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stop_all_pods(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  % just for testing 
      
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_parent_desired_state:pods(ClusterSpec)],
    timer:sleep(2000),
     [rpc:call(Pod,init,stop,[],5000)||Pod<-db_pod_desired_state:pods(ClusterSpec)],
    timer:sleep(1000),
			
    ok.

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_appl(ApplSpeList)->
    create_appl(ApplSpeList,[]).
create_appl([],Acc)->
    Acc;
create_appl([{PodNode,ApplSpec,App}|T],Acc)->
    Result=appl_server:create_appl(ApplSpec,PodNode),
    io:format("Ping  ~p~n",[{rpc:call(PodNode,App,ping,[],2000),PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Creat Appl Result ~p~n",[{Result,PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_pod_appl(ClusterSpec,AppToStart)->
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),    
    StoppedAppToStart=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    AppToStart==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedAppToStart,[])],
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),    
    StoppedCommon=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    common==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],
    StoppedSd=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    sd==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],

    % config db_etcd

    StoppedDbEtcd=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   db_etcd==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedDbEtcd,[])], 

   
    {ok,ActiveApplsInfoList_1}=appl_server:active_appls(),

    
%%-- nodelog -------------------------------------------------------------
    StoppedNodelog=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    nodelog==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedNodelog,[])],

    {ok,ActiveApplsInfoList_2}=appl_server:active_appls(),
    [{NodelogNode,NodelogApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList_2,
					    nodelog==App],
    false=rpc:call(NodelogNode,nodelog,is_config,[],5000),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,NodelogNode),
    PathLogDir=filename:join(PodDir,?LogDir),
    rpc:call(NodelogNode,file,del_dir_r,[PathLogDir],5000),
    ok=rpc:call(NodelogNode,file,make_dir,[PathLogDir],5000),
    PathLogFile=filename:join([PathLogDir,?LogFileName]),
    ok=rpc:call(NodelogNode,nodelog,config,[PathLogFile],5000),
    true=rpc:call(NodelogNode,nodelog,is_config,[],5000),
    true=lists:keymember(nodelog,3,ActiveApplsInfoList_2),
    %%------------- db_etcd
    [{DbEtcdNode,DbEtcdApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList_1,
					  etcd==App],

    false=rpc:call(DbEtcdNode,DbEtcdApp,is_config,[],5000),
    ok=rpc:call(DbEtcdNode,DbEtcdApp,config,[],5000),
    true=rpc:call(DbEtcdNode,DbEtcdApp,is_config,[],5000),
    true=lists:keymember(db_etcd,3,ActiveApplsInfoList_1),
       
    

    %% infra_service -----------------------------------------------------------

    StoppedInfraService=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   infra_service==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedInfraService,[])],
 
    {ok,ActiveApplsInfoList_3}=appl_server:active_appls(),
    [{InfraServiceNode,InfraServiceApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList_3,
					  infra_service==App],

    DbEtcd1=sd:get_node(db_etcd),
    io:format("DbEtcd1 ~p~n",[{DbEtcd1,?MODULE,?FUNCTION_NAME}]),
    ok=application:stop(db_etcd),
    DbEtcd2=sd:get_node(db_etcd),
    io:format("DbEtcd2 ~p~n",[{DbEtcd2,?MODULE,?FUNCTION_NAME}]),

    false=rpc:call(InfraServiceNode,InfraServiceApp,is_config,[],5000),
  %  io:format("R1 ~p~n",[{R1,InfraServiceNode,InfraServiceApp,?MODULE,?FUNCTION_NAME}]),
    
    R11=rpc:call(InfraServiceNode,InfraServiceApp,config,[ClusterSpec],60*1000),
    io:format("R11 ~p~n",[{R11,InfraServiceNode,InfraServiceApp,?MODULE,?FUNCTION_NAME}]),

    true=rpc:call(InfraServiceNode,InfraServiceApp,is_config,[],5000),
  %  io:format("R12 ~p~n",[{R12,InfraServiceNode,InfraServiceApp,?MODULE,?FUNCTION_NAME}]),

    ParentDS=sd:call(db_etcd,db_parent_desired_state,get_all_id,[],5000),
    io:format("ParentDS ~p~n",[{ParentDS,?MODULE,?FUNCTION_NAME,?LINE}]),
    PodDS=sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000),
    io:format("PodDS ~p~n",[{PodDS,?MODULE,?FUNCTION_NAME,?LINE}]),
    
    application:stop(infra_service),
      
    true=lists:keymember(infra_service,3,ActiveApplsInfoList_3),
    %%%
      
     ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    io:format("Stopped Parents ~p~n",[{parent_server:stopped_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Stopped Pods ~p~n",[{pod_server:stopped_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Stopped Appls ~p~n",[{appl_server:stopped_appls(),?MODULE,?FUNCTION_NAME,?LINE}]),

    

    % 1). check and restart stopped parents
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    [{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
								   Pod2<-ActiveParents,
								   Pod1/=Pod2],
    % 2). check and restart stopped pods
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    [create_pod(Pod)||Pod<-StoppedPods],
    [rpc:call(Pod1,net_adm,ping,[Pod2],5000)||Pod1<-ActiveParents,
					      Pod2<-StoppedPods,
					      Pod1/=Pod2],
    % 3). check and restart stopped appls
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    StoppedCommon=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   common==App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],
    StoppedResourceDiscovery=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   resource_discovery==App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedResourceDiscovery,[])],
    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     common/=App,
						     resource_discovery/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],

    % Radnomly kill a node or parent 


    % Wait 30 seconds to redo
    timer:sleep(20*1000),
    orchistrate().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
install_appls()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    {ok,[]}=appl_server:active_appls(),

    %-- StoppedApplInfo={PodNode,ApplSpec,App}
    % Load and Start common 
    StoppedCommon=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   common==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],
    % Load and Start resource_discovery
    StoppedResourceDiscovery=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   resource_discovery==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedResourceDiscovery,[])],

    % Load and Start nodelog  
    % Load and Start db_etcd 
    % Load and star infra_service

    % Load and start applications

    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     common/=App,
						     resource_discovery/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],

    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
secure_parents_pods_started()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
      
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_pod_desired_state:get_all_id()],
    timer:sleep(1000),
    
    %------
    
    
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [ok,ok]=[parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    [{pong,pong},{pong,pong}]=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
							  Pod2<-ActiveParents,
							  Pod1/=Pod2],
    {ok,[_,_]}=parent_server:active_nodes(),
    {ok,[]}=parent_server:stopped_nodes(),
    
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]=[create_pod(Pod)||Pod<-StoppedPods],
    [rpc:call(Pod1,net_adm,ping,[Pod2],5000)||Pod1<-ActiveParents,
					      Pod2<-StoppedPods,
					      Pod1/=Pod2],
    
    {ok,ActivePods}=pod_server:active_nodes(),
    [
     '1_c200_c201_pod@c200','1_c200_c201_pod@c201',
     '2_c200_c201_pod@c200','2_c200_c201_pod@c201',
     '3_c200_c201_pod@c200','3_c200_c201_pod@c201',
     '4_c200_c201_pod@c200','4_c200_c201_pod@c201',
     '5_c200_c201_pod@c200','5_c200_c201_pod@c201',
     '6_c200_c201_pod@c200','6_c200_c201_pod@c201'
    ]=lists:sort(ActivePods),
    {ok,[]}=pod_server:stopped_nodes(),

    io:format("nodes ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME}]),
    
    ok.
create_pod(PodNode)->
    {ok,ParentNode}=db_pod_desired_state:read(parent_node,PodNode),
    {ok,NodeName}=db_pod_desired_state:read(node_name,PodNode),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,PodNode),
    {ok,PaArgsList}=db_pod_desired_state:read(pa_args_list,PodNode),
    {ok,EnvArgs}=db_pod_desired_state:read(env_args,PodNode),
    pod_server:create_pod(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(LocalTypes,[oam,nodelog,db_etcd]).
-define(TargetTypes,[oam,nodelog,db_etcd]).

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.
