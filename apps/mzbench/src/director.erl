-module(director).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("types.hrl").
-include("ast.hrl").

-record(state, {
    super_pid = undefined,
    pools     = []
}).

start_link(SuperPid, ScriptFileName) ->
    lager:info("[ director ] Loading ~p", [ScriptFileName]),
    case read_script(ScriptFileName) of
        {ok, Script} ->
            case extract_pools(Script) of
                {ok, Pools} ->
                    gen_server:start_link(?MODULE, [SuperPid, Pools], []);
                {error, E} ->
                    {error, {shutdown, E}}
            end;
        {error, E} ->
            {error, {shutdown, E}}
    end.

init([SuperPid, Pools]) ->
    gen_server:cast(self(), {start_pools, Pools}),
    {ok, #state{
        super_pid = SuperPid
    }}.

handle_cast({start_pools, Pools}, State) ->
    {noreply, State#state{
        pools = start_pools(State#state.super_pid, Pools, [])
    }}.

handle_info({'DOWN', Ref, _, Pid, _Reason}, State) ->
    case lists:delete({Pid, Ref}, State#state.pools) of
        [] ->
            lager:info("[ director ] All pools have finished, stopping director_sup ~p", [State#state.super_pid]),
            director_sup:terminate(State#state.super_pid),
            {noreply, State};
        Pools ->
            {noreply, State#state{pools = Pools}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

start_pools(_, [], Res) ->
    lager:info("[ director ] Started all pools"),
    Res;
start_pools(SuperPid, [Pool | Pools], Res)->
    {ok, Pid} = director_sup:start_child(SuperPid, worker_pool, [SuperPid, Pool]),
    Ref = erlang:monitor(process, Pid),
    start_pools(SuperPid, Pools, [{Pid, Ref} | Res]).


-spec read_script(string()) -> {ok, [script_expr()]} | {error, any()}.
read_script(ScriptFileName) ->
    {ok, Contents} = file:read_file(ScriptFileName),
    case erl_scan:string(binary_to_list(Contents)) of
        {ok, Ts, _} ->
            {ok, [AST]} = erl_parse:parse_exprs(Ts),
            Script = ast:transform(AST),
            {ok, Script};
        {error, {_, erl_parse, E}, _} ->
            lager:error("Parsing script file failed: ~p", [E]),
            {error, E};
        A ->
            lager:error("Reading script file failed: ~p", [A]),
            {error, A}
    end.

-spec extract_pools([script_expr()]) -> {ok, [#operation{}]} | {error, any()}.
extract_pools(Script) ->
    Pools = lists:zipwith(fun(Number, #operation{name = pool, meta = Meta} = Op) ->
                                  Op#operation{meta = [{pool_name, "pool" ++ integer_to_list(Number)} | Meta]}
                          end,
                          lists:seq(1, length(Script)),
                          Script),
    Errors = lists:flatmap(fun validate_pool/1, Pools),
    case Errors of
        [] -> {ok, Pools};
        _  ->
            lager:error("Script has errors: ~p", [Errors]),
            {error, Errors}
    end.

-spec validate_pool(#operation{}) -> [string()].
validate_pool(#operation{name = pool, args = [Opts, Script]} = Op) ->
    Name = proplists:get_value(pool_name, Op#operation.meta),
    [Worker] = mproplists:get_value(worker_type, Opts),
    [Size] = mproplists:get_value(size, Opts),
    lists:map(
      fun(Msg) -> Name ++ ": " ++ Msg end,
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
