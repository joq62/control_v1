%%%-------------------------------------------------------------------
%% @doc control public API
%% @end
%%%-------------------------------------------------------------------

-module(control_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok,ClusterSpec}=sd:call(etcd,db_pod_desired_state,read,[cluster_spec,node()],5000),
    {ok,_}=control_sup:start_link(ClusterSpec).


stop(_State) ->
    ok.

%% internal functions
