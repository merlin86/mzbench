-module(worker_sup).

-export([start_link/0,
         start_worker/3
        ]).

-behaviour(supervisor).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Spec, Script, WorkerModule) ->
    supervisor:start_child(?MODULE, [Spec, Script, WorkerModule]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    AChild = {worker, {worker_runner, start_link, []}, temporary, 1000, worker, []},
    {ok, {{simple_one_for_one, 5, 1}, [AChild]}}.
