%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : §
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(monkey).      
 
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
start([ClusterSpec,AllPods])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

  

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
reboot_host(Node)->
    rpc:call(Node,os,cmd,["reboot "],5000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
kill_pod(Node)->
    rpc:call(Node,init,stop,[],5000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
is_wanted_state()->
    ParentState=case rpc:call(node(),parent_server,stopped_nodes,[],10*1000) of
		    {ok,[]}->
			true;
		    _->
			false
		end,
    PodState=case rpc:call(node(),pod_server,stopped_nodes,[],25*1000)  of
		    {ok,[]}->
			true;
		    _->
			false
		end,
    ApplState=case rpc:call(node(),appl_server,stopped_appls,[],15*1000)  of
		 {ok,[]}->
		     true;
		 _->
		     false
		end,
    ParentState and PodState and ApplState. 
