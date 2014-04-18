-module(worker_script_validator).

-export([validate_worker_script/2]).

-type expr() :: term().
-type validation_result() :: ok | {invalid_script, [string()]}.

-spec validate_worker_script([expr()], module()) -> validation_result().
validate_worker_script(Script, WorkerModule) ->
    WorkerFns = WorkerModule:module_info(exports),
    Errors = lists:flatmap(fun(Expr) ->
                                   validate_expr(Expr, WorkerFns)
                           end,
                           Script),
    case Errors of
        [] -> ok;
        _ -> {invalid_script, Errors}
    end.

-spec validate_expr(expr(), [tuple()]) -> [string()].
validate_expr(ExprTuple, WorkerFns) when is_tuple(ExprTuple) ->
    case tuple_to_list(ExprTuple) of
        [] -> ["Empty instruction."];
        [loop, LoopSpec, Body] ->
            validate_loopspec(LoopSpec) ++
            lists:flatmap(fun(Expr) ->
                                  validate_expr(Expr, WorkerFns)
                          end,
                          Body);
        [loop | _] -> ["Loop must have a spec and a body."];
        [Fn | Params] when is_atom(Fn) ->
            case proplists:get_value(Fn, WorkerFns) of
                undefined -> ["Unknown function " ++ atom_to_list(Fn) ++ "."];
                N when N =:= length(Params) + 1 -> [];
                false -> ["Arity mismatch for " ++ atom_to_list(Fn) ++ "."]
            end;
        [_ | _] -> ["First element of a tuple is not an atom."]
    end;
validate_expr(_Value, _WorkerFns) -> [].

-spec validate_loopspec([tuple()]) -> [string()].
validate_loopspec(_LoopSpec) ->
    % TODO
    [].
