-module(worker_script_validator).

-export([validate_worker_script/2]).

-include("types.hrl").
-include("ast.hrl").

-spec validate_worker_script([#operation{}], module())
    -> script_validation_result().
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

-spec validate_expr(#operation{}, [tuple()]) -> [string()].
validate_expr(#operation{} = Op, WorkerFns) ->
    case Op of
        #operation{name = undefined} -> ["Empty instruction."];
        #operation{name = loop, args = [Spec, Body]} ->
            validate_loopspec(Spec) ++
            lists:flatmap(fun(Expr) ->
                                  validate_expr(Expr, WorkerFns)
                          end,
                          Body);
        #operation{name = loop} -> ["Loop must have a spec and a body."];
        _ ->
            case proplists:get_value(Op#operation.name, WorkerFns) of
                undefined -> ["Unknown function " ++ atom_to_list(Op#operation.name) ++ "."];
                N when N =:= length(Op#operation.args) + 2 -> [];
                _ -> ["Arity mismatch for " ++ atom_to_list(Op#operation.name) ++ "."]
            end
    end;
validate_expr(_Value, _WorkerFns) -> [].

-spec validate_loopspec(#operation{}) -> [string()].
validate_loopspec(_LoopSpec) ->
    % TODO
    [].
