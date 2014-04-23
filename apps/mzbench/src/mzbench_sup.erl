-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("types.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    lager:info("Script filename: ~p~n", [ScriptFileName]),
    case file:consult(ScriptFileName) of
        {ok, [Script]} ->
            Pools = extract_pools(Script),
            lager:info("Extracted pools: ~p~n", [Pools]),

            Errors = lists:flatmap(fun validate_pool/1, Pools),

            case Errors of
                [] -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools]);
                _ -> lager:error("Script has errors: ~p", [Errors]),
                     {error, {shutdown, Errors}}
            end;
        {error, {_, erl_parse, E}} ->
            lager:error("Parsing script file failed: ~p", [E]),
            {error, {shutdown, E}};
        A ->
            lager:error("Reading script file failed: ~p", [A]),
            {error, {shutdown, A}}
    end.

init([Pools]) ->
    WorkerSupSpec = {workers,
                     {mzbench_worker_sup, start_link, []},
                     permanent,
                     5000,
                     supervisor,
                     [mzbench_worker_sup]},
    {ok, {{one_for_one, 5, 10},
          [WorkerSupSpec | lists:map(fun make_pool_child_spec/1, Pools)]
         }}.

-spec extract_pools([script_expr()]) -> [named_pool()].
extract_pools(Script) ->
    enumerate_pools(lists:map(fun parse_pool/1, Script)).

-spec parse_pool({pool, [script_expr()], [script_expr()]}) -> pool().
parse_pool({pool, PoolSpec, WorkerScript}) ->
    {literals:convert(WorkerScript),
     proplists:get_value(worker_type, PoolSpec),
     proplists:get_value(size, PoolSpec, 1)}.

-spec enumerate_pools([pool()]) -> [named_pool()].
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
      [Name, PoolOpts, Script]},
     temporary,
     5000,
     worker,
     [mzbench_pool]}.

-spec validate_pool(named_pool()) -> [string()].
validate_pool({PoolName, Script, Worker, Size}) ->
    lists:map(
      fun(Msg) -> atom_to_list(PoolName) ++ ": " ++ Msg end,
      case module_exists(Worker) of
          true ->
              ["zero size is not allowed." || Size == 0] ++
              case worker_script_validator:validate_worker_script(Script, Worker) of
                  ok -> [];
                  {invalid_script, Errors} -> Errors
              end;
          false -> ["unknown worker module."]
      end).

-spec module_exists(module()) -> boolean().
module_exists(Module) ->
    try Module:module_info() of
        _InfoList ->
            true
    catch
        _:_ ->
            false
    end.
