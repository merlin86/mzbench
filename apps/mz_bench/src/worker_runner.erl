-module(worker_runner).

-export([start_link/5, run_worker_script/4]).

% For Common and EUnit tests
-export([eval_expr/4]).

-include("types.hrl").
-include("ast.hrl").

start_link(Node, Script, Env, WorkerModule, Pool) ->
    {ok, proc_lib:spawn_link(Node, ?MODULE, run_worker_script, [Script, Env, WorkerModule, Pool])}.

-spec run_worker_script([script_expr()], worker_env() , module(), Pool :: pid())
    -> ok.

run_worker_script(Script, Env, WorkerModule, Pool) ->
    Res =
        try
            {WorkerResult, _WorkerResultState} = eval_expr(Script, WorkerModule:initial_state(), Env, WorkerModule),
             %TODO: maybe call terminate_state here
            {ok, WorkerResult}
        catch
            C:E ->
                {exception, node(), {C, E, erlang:get_stacktrace()}}
        end,

    Pool ! {worker_result, self(), Res},
    ok.

-spec eval_expr(script_expr(), worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval_expr(#operation{} = Op, State, Env, WorkerModule) ->
    eval_function(Op, State, Env, WorkerModule);
eval_expr(ExprList, State, Env, WorkerModule) when is_list(ExprList) ->
  lists:foldl(fun(E, {EvaluatedParams, CurrentState}) ->
                {EP, S} = eval_expr(E, CurrentState, Env, WorkerModule),
                {EvaluatedParams ++ [EP], S}
              end,
              {[], State},
              ExprList);
eval_expr(Value, State, _Env,  _) -> {Value, State}.

%% TODO: Implement builtin special forms and functions in separate module
%% (worker_script_stdlib or something like that).
%% This way eval_functions will be concerned with just evaluating
%% function parameters and dispatching.
-spec eval_function(#operation{}, worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval_function(#operation{name = loop} = Op, State, Env, WorkerModule) ->
  [LoopSpec, Body] = Op#operation.args,
  [#constant{value = Rate, units = rps}] = mproplists:get_value(rate, LoopSpec, [#constant{value = undefined, units = rps}]),
  [#constant{value = Time, units = ms}] = mproplists:get_value(time, LoopSpec, [#constant{value = undefined, units = ms}]),
  if
    Rate == 0 -> {nil, State};
    true -> timerun(msnow(), Time, 0, Rate, Body, State, Env, WorkerModule)
  end;
eval_function(#operation{name = choose, args = Args}, State, Env, _) -> choose(Args, State, Env);
eval_function(#operation{name = random_binary, args = [N]}, State, _Env, _) -> {utility:random_binary(N), State};
eval_function(#operation{name = random_list, args = [N]}, State, _Env, _) -> {utility:random_list(N), State};
eval_function(#operation{name = use_graphite, args = [Addr]}, State, _Env, _) ->
  {H, P} = case string:tokens(Addr, ":") of
    [Host]       -> {Host, 2003};
    [Host, Port] -> {Host, list_to_integer(Port)}
  end,
  application:set_env(mz_bench, graphite_port, P),
  application:set_env(mz_bench, graphite_host, H),
  {nil, State};
eval_function(#operation{} = Op, State, Env, WorkerModule) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval_expr(Op#operation.args, State, Env, WorkerModule),
    {Res, NewState} = apply(WorkerModule, Op#operation.name, [NextState, Op#operation.meta | Params]),
    {Res, NewState}.

choose([#operation{name = resource, args = [Name]}], State, Env) ->
    List = proplists:get_value({resource, Name}, Env, []),
    {utility:choose(List), State};
choose([N, List], State, _Env) -> {utility:choose(N, List), State};
choose([List], State, _Env) -> {utility:choose(List), State}.

msnow() ->
  {MegaSecs, Secs, MicroSecs} = os:timestamp(),
  trunc(MegaSecs * 1000000000 + Secs * 1000 + MicroSecs / 1000).

timerun(Start, Time, Done, Rate, Expr, State, Env, WorkerModule) ->
  LocalStart = msnow(),
  if
    (Time =/= undefined) and (Time + Start =< LocalStart) -> {nil, State};
    true ->
      {_, NextState} = eval_expr(Expr, State, Env, WorkerModule),
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
      timerun(Start, Time, Done+1, Rate, Expr, NextState, Env, WorkerModule)
  end.
