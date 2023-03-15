%%%-------------------------------------------------------------------
%% @doc control public API
%% @end
%%%-------------------------------------------------------------------

-module(control_app).

-behaviour(application).

-export([start/2, stop/1]).
-define(Application,control).

start(_StartType, _StartArgs) ->
    case application:get_env(?Application,cluster_spec) of
	undefined->
	    {error,[cluster_spec, undefined]};
	{ok,ClusterSpec}->
	    control_sup:start_link(ClusterSpec)
    end.


stop(_State) ->
    ok.

%% internal functions
