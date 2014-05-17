-module(worker_script_validator).

-export([validate_worker_script/2]).

-include("types.hrl").
-include("ast.hrl").

-spec validate_worker_script([#operation{}], module())
    -> script_validation_result().
validate_worker_script(Script, WorkerModule) ->
    WorkerFns = WorkerModule:module_info(exports),
    Errors = lists:flatmap(fun(Expr) ->
                                   validate_expr(Expr, WorkerFns, WorkerModule)
                           end,
                           Script),
    case Errors of
        [] -> ok;
        _ -> {invalid_script, Errors}
    end.

-spec validate_expr(#operation{}, [tuple()], module()) -> [string()].
validate_expr(#operation{} = Op, WorkerFns, WorkerModule) ->
    Meta = Op#operation.meta,
    LocInfo = fun() -> case proplists:get_value(line, Meta) of
                           undefined -> "";
                           LineNumber -> "line " ++ integer_to_list(LineNumber) ++ ": "
                       end
              end,
    case Op of
        #operation{name = undefined} -> [LocInfo() ++ "Empty instruction."];
        #operation{name = loop, args = [Spec, Body]} ->
            validate_loopspec(Spec) ++
            lists:flatmap(fun(Expr) ->
                                  validate_expr(Expr, WorkerFns, WorkerModule)
                          end,
                          Body);
        #operation{name = loop} -> [LocInfo() ++ "Loop must have a spec and a body."];
        _ ->
            Fn = Op#operation.name,
            Arity = length(Op#operation.args) + 2,
            case lists:member({Fn, Arity}, WorkerFns) of
                true -> [];
                false ->
                    [lists:flatten(io_lib:format(LocInfo() ++ "Unknown function ~p:~p/~p", [WorkerModule, Fn, Arity]))]
            end
    end;
validate_expr(_, _, _) -> [].

-spec validate_loopspec(#operation{}) -> [string()].
validate_loopspec(_LoopSpec) ->
    % TODO
    [].
