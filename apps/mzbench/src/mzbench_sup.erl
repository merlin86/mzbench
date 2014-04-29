-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("types.hrl").
-include("ast.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link(ScriptFileName) ->
    lager:info("Script filename: ~p~n", [ScriptFileName]),
    {ok, Contents} = file:read_file(ScriptFileName),
    case erl_scan:string(binary_to_list(Contents)) of
        {ok, Ts, _} ->
            {ok, [AST]} = erl_parse:parse_exprs(Ts),
            Script = ast:transform(AST),
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

-spec extract_pools([script_expr()]) -> [#pool{name::atom(), script::[script_expr()]}].
extract_pools(Script) ->
    lists:zipwith(
      fun(Number, #operation{name = pool} = Op) ->
              [PoolOpts, PoolScript] = Op#operation.args,
              #pool{name = list_to_atom("pool" ++ integer_to_list(Number)),
                    opts = PoolOpts,
                    script = PoolScript,
                    meta = Op#operation.meta
                    }
      end,
      lists:seq(1, length(Script)),
      Script).

-spec make_pool_child_spec(#pool{}) -> supervisor:child_spec().
make_pool_child_spec(#pool{} = P) ->
    {P#pool.name,
     {mzbench_pool,
      start_link,
      [P#pool.name, P#pool.opts, P#pool.script]},
     temporary,
     5000,
     worker,
     [mzbench_pool]}.

-spec validate_pool(#pool{}) -> [string()].
validate_pool(#pool{} = P) ->
    [Worker] = mproplists:get_value(worker_type, P#pool.opts),
    [Size] = mproplists:get_value(size, P#pool.opts),
    lists:map(
      fun(Msg) -> atom_to_list(P#pool.name) ++ ": " ++ Msg end,
      case module_exists(Worker) of
          true ->
              ["zero size is not allowed." || Size == 0] ++
              case worker_script_validator:validate_worker_script(P#pool.script, Worker) of
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
