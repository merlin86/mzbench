-module(worker_runner).

-export([start_link/3, run_worker_script/3]).

-include("types.hrl").

start_link(Spec, Script, WorkerModule) ->
    {ok, proc_lib:spawn_link(?MODULE, run_worker_script, [Spec, Script, WorkerModule])}.

%% Spec is supposed to contain worker id, launch timestamp
%% and possibly some way to notify parent.
%% Feel free to add when necessary.
-spec run_worker_script(term(), [script_expr()], module())
    -> {ok, worker_state()}.
run_worker_script(_Spec, Script, WorkerModule) ->
    {_, WorkerResultState} = eval_expr(Script, WorkerModule:initial_state(), WorkerModule),
    {ok, WorkerResultState}.

-spec eval_expr(script_expr(), worker_state(), module())
    -> {script_value(), worker_state()}.
eval_expr(ExprTuple, State, WorkerModule) when is_tuple(ExprTuple) ->
    eval_function(tuple_to_list(ExprTuple), State, WorkerModule);
eval_expr(ExprList, State, WorkerModule) when is_list(ExprList) ->
  lists:foldl(fun(E, {EvaluatedParams, CurrentState}) ->
                {EP, S} = eval_expr(E, CurrentState, WorkerModule),
                {EvaluatedParams ++ [EP], S}
              end,
              {[], State},
              ExprList);
eval_expr(Value, State, _) -> {Value, State}.

%% TODO: Implement builtin special forms and functions in separate module
%% (worker_script_stdlib or something like that).
%% This way eval_functions will be concerned with just evaluating
%% function parameters and dispatching.
-spec eval_function([script_expr()], worker_state(), module())
    -> {script_value(), worker_state()}.
eval_function([loop, LoopSpec, Body], State, WorkerModule) ->
  {Rate, rps} = proplists:get_value(rate, LoopSpec, {undefined, rps}),
  {Time, ms} = proplists:get_value(time, LoopSpec, {undefined, ms}),
  Interval = if
               Rate == undefined -> 0;
               Rate == 0 -> undefined;
               true -> 1000/Rate
             end,
  if
    Interval == undefined -> {nil, State};
    true -> timerun(msnow(), Time, trunc(Interval), Body, State, WorkerModule)
  end;
eval_function([choose, List], _, _) -> utility:choose(List);
eval_function([choose, N, List], _, _) -> utility:choose(N, List);
eval_function([random_bytes, N], _, _) -> utility:random_bytes(N);
eval_function([FnName | ParamExprs], State, WorkerModule) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval_expr(ParamExprs, State, WorkerModule),
    apply(WorkerModule, FnName, [NextState | Params]).


msnow() ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  trunc(MegaSecs * 1000000000 + Secs * 1000 + MicroSecs / 1000).

timerun(Start, Time, Interval, Expr, State, WorkerModule) ->
  LocalStart = msnow(),
  if
    (Time =/= undefined) and (Time + Start =< LocalStart) -> {nil, State};
    true ->
      {_, NextState} = eval_expr(Expr, State, WorkerModule),
      Remain = LocalStart + Interval - msnow(),
      if
        Remain > 0 -> timer:sleep(Remain);
        true -> ok
      end,
    timerun(Start, Time, Interval, Expr, NextState, WorkerModule)
  end.
