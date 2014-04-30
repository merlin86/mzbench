-module(mzbench_director_sup).
-export([start_link/1,
         start_child/3,
         stop/1
        ]).

-behaviour(supervisor).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ScriptFileName) ->
    supervisor:start_link(?MODULE, [ScriptFileName]).

start_child(Pid, Module, Args) ->
    supervisor:start_child(Pid, {make_ref(), {Module, start_link, Args}, temporary, 1000, worker, [Module]}).

stop(Pid) ->
    supervisor:terminate_child(mzbench_sup, Pid).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([ScriptFileName]) ->
    lager:info("[ director_sup ] I'm at ~p", [self()]),
    {ok, {{one_for_one, 5, 1}, [
        child_spec(event_exporter, transient, []),
        child_spec(mzbench_director, temporary, [self(), ScriptFileName])
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(I, Restart, Args) ->
    {I, {I, start_link, Args}, Restart, 1000, worker, [I]}.
