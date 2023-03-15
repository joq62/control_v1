%%%-------------------------------------------------------------------
%% @doc org top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(control_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ClusterSpec) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [ClusterSpec]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([ClusterSpec]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
		  #{id=>parent,
		    start=>{parent_server,start,[ClusterSpec]}},
		  #{id=>pod,
		    start=>{pod_server,start,[ClusterSpec]}},
		  #{id=>appl,
		    start=>{appl_server,start,[ClusterSpec]}},
		  #{id=>control,
		    start=>{control,start,[ClusterSpec]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
