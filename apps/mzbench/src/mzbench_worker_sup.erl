-module(mzbench_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_worker/3
        ]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(Spec, Script, WorkerModule) ->
    supervisor:start_child(?MODULE, [Spec, Script, WorkerModule]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    AChild = {worker, {worker_runner, start_link, []}, temporary, 1000, worker, []},

    {ok, {{simple_one_for_one, 100, 100}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

