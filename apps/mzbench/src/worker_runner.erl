-module(worker_runner).

-export([run_worker_script/3]).

-type state() :: term().
-type value() :: term().
-type expr() :: tuple() | value().
-type spec() :: [tuple()].

%% Spec is supposed to contain worker id, launch timestamp
%% and possibly some way to notify parent.
%% Feel free to add when necessary.
-spec run_worker_script(spec(), [expr()], module()) -> {ok, state()}.
run_worker_script(_Spec, Script, WorkerModule) ->
    WorkerResultState = lists:foldl(fun(Expr, S) ->
                                            element(2, eval_expr(Expr, S, WorkerModule))
                                    end,
                                    WorkerModule:initial_state(),
                                    Script),
    {ok, WorkerResultState}.

-spec eval_expr(expr(), state(), module()) -> {value(), state()}.
eval_expr(ExprTuple, State, WorkerModule) when is_tuple(ExprTuple) ->
    eval_function(tuple_to_list(ExprTuple), State, WorkerModule);
eval_expr(Value, State, _) -> {Value, State}.

%% TODO: Implement builtin special forms and functions in separate module
%% (worker_script_stdlib or something like that).
%% This way eval_functions will be concerned with just evaluating
%% function parameters and dispatching.
-spec eval_function([expr()], state(), module()) -> {value(), state()}.
eval_function([loop, _LoopSpec, Body], State, WorkerModule) ->
    {nil, lists:foldl(fun(E, S) -> eval_expr(E, S, WorkerModule) end,
                      State,
                      Body)};
eval_function([FnName | ParamExprs], State, WorkerModule) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = lists:foldl(fun(E, {EvaluatedParams, CurrentState}) ->
                                              {EP, S} = eval_expr(E, CurrentState, WorkerModule),
                                              {EvaluatedParams ++ [EP], S}
                                      end,
                                      {[], State},
                                      ParamExprs),
    apply(WorkerModule, FnName, [NextState | Params]).

