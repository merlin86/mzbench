-module(mzbench_director).

-export([start_link/3]).

-behaviour(gen_server).
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include("types.hrl").
-include("ast.hrl").

-record(state, {
    super_pid = undefined,
    pools     = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SuperPid, Script, Nodes) ->
    case extract_pools(Script) of
        {ok, {Pools, Env}} ->
            gen_server:start_link(?MODULE, [SuperPid, Pools, Env, Nodes], []);
        {error, E} ->
            {error, {shutdown, E}}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, Pools, Env, Nodes]) ->
    gen_server:cast(self(), {start_pools, Pools, Env, alive_nodes(Nodes)}),
    {ok, #state{
        super_pid = SuperPid
    }}.

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_pools, _Pools, _Env, []}, State) ->
    lager:error("[ director ] There are no alive nodes to start workers"),
    mzbench_director_sup:stop(State#state.super_pid),
    {stop, empty_nodes, State};
handle_cast({start_pools, Pools, Env, Nodes}, State) ->
    {noreply, State#state{
        pools = start_pools(State#state.super_pid, Pools, Env, Nodes, [])
    }};
handle_cast(Req, State) ->
    lager:error("Unhandled cast: ~p", [Req]),
    {stop, {unhandled_cast, Req}, State}.

handle_info({'DOWN', Ref, _, Pid, _Reason}, State) ->
    case lists:delete({Pid, Ref}, State#state.pools) of
        [] ->
            lager:info("[ director ] All pools have finished, stopping mzbench_director_sup ~p", [State#state.super_pid]),
            mzbench_director_sup:stop(State#state.super_pid),
            {noreply, State};
        Pools ->
            {noreply, State#state{pools = Pools}}
    end;
handle_info(Req, State) ->
    lager:error("Unhandled info: ~p", [Req]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_pools(_, [], _, _, Acc) ->
    lager:info("[ director ] Started all pools"),
    Acc;
start_pools(SuperPid, [Pool | Pools], Env, Nodes, Acc)->
    {ok, Pid} = mzbench_director_sup:start_child(SuperPid, mzbench_pool, [SuperPid, Pool, Env, Nodes]),
    Ref = erlang:monitor(process, Pid),
    start_pools(SuperPid, Pools, Env, Nodes, [{Pid, Ref} | Acc]).

-spec extract_pools([script_expr()]) -> {ok, [#operation{}]} | {error, any()}.
extract_pools(Script) ->
    Env = lists:foldl(
            fun (#operation{name = resource, args = [Name, Data]}, Acc) -> [{{resource, Name}, Data}|Acc];
                (_, Acc) -> Acc
            end, [], Script),

    Script2 = lists:filter(fun (#operation{name = pool}) -> true; (_) -> false end, Script),
    Pools = lists:zipwith(fun(Number, #operation{name = pool, meta = Meta} = Op) ->
                                  Op#operation{meta = [{pool_name, "pool" ++ integer_to_list(Number)} | Meta]}
                          end,
                          lists:seq(1, length(Script2)),
                          Script2),
    Errors = lists:flatmap(fun validate_pool/1, Pools),
    case Errors of
        [] -> {ok, {Pools, Env}};
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

-spec alive_nodes([atom()]) -> [atom()].
alive_nodes(Nodes) ->
    lists:filter(
        fun (N) ->
            net_adm:ping(N) == pong
        end, Nodes).

