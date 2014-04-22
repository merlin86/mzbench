-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ScriptFileName]).

init([ScriptFileName]) ->
    lager:info("Script filename: ~p~n", [ScriptFileName]),
    {ok, [Script]} = file:consult(ScriptFileName),
    Pools = extract_pools(Script),
    lager:info("Extracted pools: ~p~n", [Pools]),
    WorkerSupSpec = {workers,
            {mzbench_worker_sup, start_link, []},
            permanent,
            5000,
            supervisor,
            [mzbench_worker_sup]},
    {ok, {{one_for_one, 5, 10},
          [WorkerSupSpec | lists:map(fun make_pool_child_spec/1, Pools)]
         }}.

extract_pools(Script) ->
    enumerate_pools(lists:map(fun parse_pool/1, Script)).

parse_pool({pool, PoolSpec, WorkerScript}) ->
    {WorkerScript,
     proplists:get_value(worker_type, PoolSpec),
     proplists:get_value(size, PoolSpec, 1)}.

enumerate_pools(Xs) ->
    lists:zipwith(
      fun(Number, {Script, Worker, Size}) ->
              {list_to_atom("pool" ++ integer_to_list(Number)),
               Script,
               Worker,
               Size}
      end,
      lists:seq(1, length(Xs)),
      Xs).

make_pool_child_spec({Name, Script, WorkerModule, PoolSize}) ->
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
