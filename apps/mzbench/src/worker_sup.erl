-module(worker_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_worker/3
        ]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Spec, Script, WorkerModule) ->
    supervisor:start_child(?MODULE, [Spec, Script, WorkerModule]).

init([]) ->
    AChild = {worker, {worker_runner, start_link, []}, temporary, 1000, worker, []},

    {ok, {{simple_one_for_one, 100, 100}, [AChild]}}.
