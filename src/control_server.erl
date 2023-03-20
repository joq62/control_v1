%%% -------------------------------------------------------------------
%%% Author  : joqerlang
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(control_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(App,control).
%% --------------------------------------------------------------------


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       leader_pid,
	       cluster_spec,
	       wanted_state
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

       

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
%init(ClusterSpec) -> 
init(ClusterSpec) -> 
    io:format(" ~p~n",[{ClusterSpec,?MODULE,?LINE}]),
    {ok,LeaderPid}=leader:start(?App),
    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"server start",[]]),
    {ok, #state{cluster_spec=ClusterSpec,
		leader_pid=LeaderPid,
		wanted_state=undefined},0}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% Leader API functions
handle_call({am_i_leader,Node}, _From, State) ->
    Reply = leader:am_i_leader(State#state.leader_pid,Node,5000),
    {reply, Reply, State};

handle_call({who_is_leader}, _From, State) ->
    Reply = leader:who_is_leader(State#state.leader_pid,5000),
    {reply, Reply, State};

handle_call({ping_leader}, _From, State) ->
    Reply = leader:ping(State#state.leader_pid,5000),
    {reply, Reply, State};

%% gen_server API
handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Request,From]]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% Leader API functions
handle_cast({i_am_alive,MyNode}, State) ->
    leader:i_am_alive(State#state.leader_pid,MyNode),
    {noreply, State};

handle_cast({declare_victory,LeaderNode}, State) ->
    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"declare_victory from ",[LeaderNode]]),
    leader:declare_victory(State#state.leader_pid,LeaderNode),
    {noreply, State};

handle_cast({start_election}, State) ->
    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start_election ",[]]),
    leader:start_election(State#state.leader_pid),
    {noreply, State};

%% gen_server API functions

handle_cast({orchestrate_result,
	     ResultStartParents,
	     ResultStartPods,
	     ResultStartInfraAppls,
	     ResultStartUserAppls}, State) ->

    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"start orchestrate_result ",[]]),

    StoppedParents=rpc:call(node(),parent_server,stopped_nodes,[],5*1000),
    case StoppedParents of
	{ok,_}->
	    ok;
	Reason1->
	    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Error calling for StoppedParents ",[StoppedParents,Reason1]])
    end,
	
    StoppedPod=rpc:call(node(),pod_server,stopped_nodes,[],5*1000),
    case StoppedPod of
	{ok,_}->
	    ok;
	Reason2->
	    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Error calling for StoppedPod ",[StoppedPod,Reason2]])
    end,
    StoppedAppl=rpc:call(node(),appl_server,stopped_appls,[],5*1000),
    case StoppedAppl of
	{ok,_}->
	    ok;
	Reason3->
	    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Error calling for StoppedAppl ",[StoppedAppl,Reason3]])
    end,
    
    NewWantedState=case {StoppedParents,StoppedPod,StoppedAppl} of
		       {{ok,[]},{ok,[]},{ok,[]}}->
			   desired_state;
		       _->
			   not_desired_state
		   end,
    case State#state.wanted_state of
	NewWantedState->
	    ok;
	_->	 
	    sd:cast(log,log,notice,[?MODULE,?FUNCTION_NAME,?LINE,node(),
				    "New State ",[NewWantedState,
						  ResultStartParents,
						  ResultStartPods,
						  ResultStartInfraAppls,
						  ResultStartUserAppls]])   
    end,

  
    rpc:cast(node(),orchestrate,start,[State#state.cluster_spec,
				       State#state.leader_pid]),
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"End - calling  start orchestrate ",[]]),
    {noreply, State#state{wanted_state=NewWantedState}};

handle_cast(Msg, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Msg]]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |

%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State) ->
    rpc:cast(node(),orchestrate,start,[State#state.cluster_spec,
				       State#state.leader_pid]),
    sd:cast(log,log,debug,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Server start timeout",[State#state.leader_pid]]),
    {noreply, State};

handle_info(Info, State) ->
    sd:cast(log,log,warning,[?MODULE,?FUNCTION_NAME,?LINE,node(),"Unmatched signal",[Info]]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
