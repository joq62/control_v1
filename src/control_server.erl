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


%% --------------------------------------------------------------------


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
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
init(ClusterSpec) -> 
    io:format(" ~p~n",[{ClusterSpec,?MODULE,?LINE}]),
    Result=rpc:cast(node(),orchestrate,start,[ClusterSpec]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Servere started",Result,node()]]),
    {ok, #state{cluster_spec=ClusterSpec,
		wanted_state=undefined}}.   
 

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
handle_call({start_orchistrate},_From, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["start_orchistrate",time(),node()]]),
    Reply= rpc:cast(node(),orchestrate,start,[State#state.cluster_spec]),
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ping",node()]]),
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Request,?MODULE,?LINE]]),
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({orchestrate_result,
	     ResultStartParents,
	     ResultStartPods,
	     ResultStartInfraAppls,
	     ResultStartUserAppls}, State) ->
    
    
    {ok,StoppedParents}=lib_parent:stopped_nodes(State#state.cluster_spec),
    {ok,StoppedPod}=lib_pod:stopped_nodes(State#state.cluster_spec),
    {ok,StoppedAppl}=lib_appl:stopped_nodes(State#state.cluster_spec),
    
    NewWantedState=case {StoppedParents,StoppedPod,StoppedAppl} of
		       {[],[],[]}->
			   desired_state;
		       _->
			   not_desired_state
		   end,
    case State#state.wanted_state of
	NewWantedState->
	    ok;
	_->	    
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["New state:",date(),time(),
								      NewWantedState,
								      ResultStartParents,
								      ResultStartPods,
								      ResultStartInfraAppls,
								      ResultStartUserAppls,
								      ?MODULE,?LINE]])
    end,
    rpc:cast(node(),orchestrate,start,[State#state.cluster_spec]),
    {noreply, State#state{wanted_state=NewWantedState}};

handle_cast(Msg, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Msg,?MODULE,?LINE]]),
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Info,?MODULE,?LINE]]),
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
