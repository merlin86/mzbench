-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ScriptFileName]).

init([ScriptFileName]) ->
    {ok, { {one_for_one, 5, 10},
    [
        {workers, {mzbench_worker_sup, start_link, []}, permanent, 5000, supervisor, [mzbench_worker_sup]}
    ]} }.

