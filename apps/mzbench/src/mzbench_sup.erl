-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ScriptFileName]).

init([ScriptFileName]) ->
    lager:info("Script filename: ~p~n", [ScriptFileName]),
    {ok, Script} = file:consult(ScriptFileName),
    Pools = extract_pools(Script),
    lager:info("Extracted pools: ~p~n", [Pools]),
    WorkerSupSpec = {workers,
            {mzbench_worker_sup, start_link, []},
            permanent,
            5000,
            supervisor,
            [mzbench_worker_sup]},
    {ok, {{one_for_one, 5, 10},
          [WorkerSupSpec | lists:map(fun start_pool/1, Pools)]
         }}.

extract_pools(_Script) ->
    % TODO: unhardcode
    [{pool1,
      [{loop, [{time, {3, min}},
               {rate, {1, rps}}],
        [{print, "FOO"}]}],
      dummy_worker,
      2},
     {pool2,
      [{print, "OHAI"}, {print, "BAR"}],
      dummy_worker,
      3}].

start_pool({Name, Script, WorkerModule, PoolSize}) ->
    PoolOpts = [{size, PoolSize},
                {worker, WorkerModule}],
    {Name,
     {mzbench_pool,
      start_link,
      [pool_name, PoolOpts, Script]},
     temporary,
     5000,
     worker,
     [mzbench_pool]}.
