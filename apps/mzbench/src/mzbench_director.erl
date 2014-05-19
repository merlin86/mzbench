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

start_link(SuperPid, ScriptFileName, Nodes) ->
    lager:info("[ director ] Loading ~p", [ScriptFileName]),
    case read_script(ScriptFileName) of
        {ok, Script} ->
            case extract_pools(Script) of
                {ok, Pools} ->
                    gen_server:start_link(?MODULE, [SuperPid, Pools, Nodes], []);
                {error, E} ->
                    {error, {shutdown, E}}
            end;
        {error, E} ->
            {error, {shutdown, E}}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, Pools, Nodes]) ->
    gen_server:cast(self(), {start_pools, Pools, alive_nodes(Nodes)}),
    {ok, #state{
        super_pid = SuperPid
    }}.

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_pools, Pools, Nodes}, State) ->
    {noreply, State#state{
        pools = start_pools(State#state.super_pid, Pools, Nodes, [])
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

start_pools(_, [], _, Acc) ->
    lager:info("[ director ] Started all pools"),
    Acc;
start_pools(SuperPid, [Pool | Pools], Nodes, Acc)->
    {ok, Pid} = mzbench_director_sup:start_child(SuperPid, mzbench_pool, [SuperPid, Pool, Nodes]),
    Ref = erlang:monitor(process, Pid),
    start_pools(SuperPid, Pools, Nodes, [{Pid, Ref} | Acc]).

-spec read_script(string()) -> {ok, [script_expr()]} | {error, any()}.
read_script(ScriptFileName) ->
    {ok, Contents} = file:read_file(ScriptFileName),
    case erl_scan:string(binary_to_list(Contents)) of
        {ok, Ts, _} ->
            case erl_parse:parse_exprs(Ts) of
                {ok, [AST]} ->
                    Script = ast:transform(AST),
                    RunId = make_run_id(ScriptFileName),
                    FinalScript = ast:add_meta(Script, [{run_id, RunId}]),
                    {ok, FinalScript};
                {error, E} ->
                    lager:error("Parsing script file failed: ~p", [E]),
                    {error, E}
            end;
        {error, E, _} ->
            lager:error("Scanning script file failed: ~p", [E]),
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

-spec alive_nodes([atom()]) -> [atom()].
alive_nodes(Nodes) ->
    lists:filter(
        fun (N) ->
            net_adm:ping(N) == pong
        end, Nodes).

-spec make_run_id(string()) -> string().
make_run_id(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    lists:flatten(io_lib:format("~s-~s", [Name, iso_8601_fmt(erlang:localtime())])).

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).
