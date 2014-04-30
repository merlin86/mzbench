-module(director_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/3, terminate/1]).
-export([init/1]).

-define(CHILD(I, Restart, Args), {I, {I, start_link, Args}, Restart, 1000, worker, [I]}).

start_link(ScriptFileName) ->
    supervisor:start_link(?MODULE, [ScriptFileName]).

init([ScriptFileName]) ->
    lager:info("[ director_sup ] I'm at ~p", [self()]),
    {ok, {{one_for_one, 5, 1}, [
        ?CHILD(event_exporter, transient, []),
        ?CHILD(director, temporary, [self(), ScriptFileName])
    ]}}.

start_child(Pid, Module, Args) ->
    supervisor:start_child(Pid, {make_ref(), {Module, start_link, Args}, temporary, 1000, worker, [Module]}).

terminate(Pid) ->
    supervisor:terminate_child(mzbench_sup, Pid).
