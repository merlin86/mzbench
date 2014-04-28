-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("types.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    lager:info("Script filename: ~p~n", [ScriptFileName]),
    {ok, Contents} = file:read_file(ScriptFileName),
    case erl_scan:string(binary_to_list(Contents)) of
        {ok, Ts, _} ->
            {ok, [AST]} = erl_parse:parse_exprs(Ts),
            Script = erl_parse:normalise(ast:markup(AST)),
            Pools = extract_pools(Script),
            lager:info("Extracted pools: ~p~n", [Pools]),

            Errors = lists:flatmap(fun validate_pool/1, Pools),

            case Errors of
                [] -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools]);
                _ -> lager:error("Script has errors: ~p", [Errors]),
                     {error, {shutdown, Errors}}
            end;
        {error, {_, erl_parse, E}, _} ->
            lager:error("Parsing script file failed: ~p", [E]),
            {error, {shutdown, E}};
        A ->
            lager:error("Reading script file failed: ~p", [A]),
            {error, {shutdown, A}}
    end.

init([Pools]) ->
    EventExporterSpec = {exporter,
                         {event_exporter, start_link, []},
                         temporary,
                         5000,
                         worker,
                         [event_exporter]},
    WorkerSupSpec = {workers,
                     {mzbench_worker_sup, start_link, []},
                     permanent,
                     5000,
                     supervisor,
                     [mzbench_worker_sup]},
    {ok, {{one_for_one, 5, 10},
          [EventExporterSpec] ++ [WorkerSupSpec | lists:map(fun make_pool_child_spec/1, Pools)]
         }}.

-spec extract_pools([script_expr()]) -> [named_pool()].
extract_pools(Script) ->
    enumerate_pools(lists:map(fun parse_pool/1, Script)).

-spec parse_pool({pool, [script_expr()], [script_expr()], [script_expr()]}) -> pool().
parse_pool({pool, PoolOpts, WorkerScript, _}) ->
    {literals:convert(WorkerScript),
     PoolOpts}.

-spec enumerate_pools([pool()]) -> [named_pool()].
enumerate_pools(Xs) ->
    lists:zipwith(
      fun(Number, {Script, PoolOpts}) ->
              {list_to_atom("pool" ++ integer_to_list(Number)),
               Script,
               PoolOpts}
      end,
      lists:seq(1, length(Xs)),
      Xs).

-spec make_pool_child_spec(named_pool()) -> supervisor:child_spec().
make_pool_child_spec({Name, Script, PoolOpts}) ->
    {Name,
     {mzbench_pool,
      start_link,
      [Name, PoolOpts, Script]},
     temporary,
     5000,
     worker,
     [mzbench_pool]}.

-spec validate_pool(named_pool()) -> [string()].
validate_pool({PoolName, Script, PoolOpts}) ->
    Worker = mproplists:get_value(worker_type, PoolOpts),
    Size = mproplists:get_value(size, PoolOpts),
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
