-module(worker_runner).

-export([start_link/4, run_worker_script/3]).

% For Common and EUnit tests
-export([eval_expr/3]).

-include("types.hrl").
-include("ast.hrl").

start_link(Node, Script, WorkerModule, Pool) ->
    {ok, proc_lib:spawn_link(Node, ?MODULE, run_worker_script, [Script, WorkerModule, Pool])}.

-spec run_worker_script([script_expr()], module(), Pool :: pid())
    -> {ok, worker_state()}.

run_worker_script(Script, WorkerModule, Pool) ->
    Res =
        try
            {WorkerResult, _WorkerResultState} = eval_expr(Script, WorkerModule:initial_state(), WorkerModule),
             %TODO: maybe call terminate_state here
            {ok, WorkerResult}
        catch
            C:E ->
                {exception, node(), {C, E, erlang:get_stacktrace()}}
        end,

    Pool ! {worker_result, self(), Res}.

-spec eval_expr(script_expr(), worker_state(), module())
    -> {script_value(), worker_state()}.
eval_expr(#operation{} = Op, State, WorkerModule) ->
    eval_function(Op, State, WorkerModule);
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
-spec eval_function(#operation{}, worker_state(), module())
    -> {script_value(), worker_state()}.
eval_function(#operation{name = loop} = Op, State, WorkerModule) ->
  [LoopSpec, Body] = Op#operation.args,
  [#constant{value = Rate, units = rps}] = mproplists:get_value(rate, LoopSpec, [#constant{value = undefined, units = rps}]),
  [#constant{value = Time, units = ms}] = mproplists:get_value(time, LoopSpec, [#constant{value = undefined, units = ms}]),
  if
    Rate == 0 -> {nil, State};
    true -> timerun(msnow(), Time, 0, Rate, Body, State, WorkerModule)
  end;
eval_function(#operation{name = choose, args = [N, List]}, _, _) -> utility:choose(N, List);
eval_function(#operation{name = choose, args = List}, _, _) -> utility:choose(List);
eval_function(#operation{name = random_bytes, args = N}, _, _) -> utility:random_bytes(N);
eval_function(#operation{} = Op, State, WorkerModule) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval_expr(Op#operation.args, State, WorkerModule),
    apply(WorkerModule, Op#operation.name, [NextState, Op#operation.meta | Params]).


msnow() ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  trunc(MegaSecs * 1000000000 + Secs * 1000 + MicroSecs / 1000).

timerun(Start, Time, Done, Rate, Expr, State, WorkerModule) ->
  LocalStart = msnow(),
  if
    (Time =/= undefined) and (Time + Start =< LocalStart) -> {nil, State};
    true ->
      {_, NextState} = eval_expr(Expr, State, WorkerModule),
      case Rate of
        undefined -> ok;
        _ ->
          ShouldBe = (Done + 1)*1000/Rate,
          Remain = Start + trunc(ShouldBe) - msnow(),
          if
            Remain > 0 -> timer:sleep(Remain);
            true -> ok
          end
      end,
      timerun(Start, Time, Done+1, Rate, Expr, NextState, WorkerModule)
  end.
