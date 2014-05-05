-module(mzbench_pool).

-export([start_link/3,
         stop/1
        ]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include("ast.hrl").

-record(s, {
    workers = [],
    name    = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SuperPid, Pool, Nodes) ->
    gen_server:start_link(?MODULE, [SuperPid, Pool, Nodes], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, Pool, Nodes]) ->
    Tid = ets:new(pool_workers, [protected, {keypos, 1}]),
    gen_server:cast(self(), {start_workers, SuperPid, Pool, Nodes}),
    {ok, #s{workers = Tid}}.

handle_call(stop, _From, #s{workers = Tid, name = Name} = State) ->
    lager:info("[ ~p ] Received stop signal", [Name]),
    ets:foldl(
        fun ({Pid, Ref}, Acc) ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit(Pid, kill),
            Acc
        end, [], Tid),
    ets:delete_all_objects(Tid),
    {stop, normal, ok, State = #s{}};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_workers, SuperPid, Pool, Nodes}, #s{workers = Tid} = State) ->
    #operation{name = pool, args = [PoolOpts, Script], meta = Meta} = Pool,
    Name = proplists:get_value(pool_name, Meta),
    [Size] = mproplists:get_value(size, PoolOpts, [undefined]),
    [WorkerModule] = mproplists:get_value(worker_type, PoolOpts, [undefined]),
    utility:fold_interval(
        fun (N, [NextNode|T]) ->
            WorkerScript = ast:add_meta(Script, [{worker_id, N}]),
            Args = [NextNode, WorkerScript, WorkerModule, self()],
            {ok, P} = mzbench_director_sup:start_child(SuperPid, worker_runner, Args),
            Ref = erlang:monitor(process, P),
            ets:insert(Tid, {P, Ref}),
            T ++ [NextNode]
        end, Nodes, 1, Size),
    lager:info("[ ~p ] Started ~p workers", [Name, Size]),
    {noreply, State#s{name = Name}};

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info({worker_result, Pid, Res}, #s{workers = Workers, name = Name} = State) ->

    maybe_report_error(Pid, Res),

    case ets:lookup(Workers, Pid) of
        [{Pid, Ref}] ->
            ets:delete(Workers, Pid),
            erlang:demonitor(Ref, [flush]);
        _ ->
            lager:error("[ ~p ] Received result from unknown worker: ~p / ~p", [Name, Pid, Res])
    end,
    maybe_stop(State);

handle_info({'DOWN', _Ref, _, Pid, Reason}, #s{workers = Workers, name = Name} = State) ->
    case ets:lookup(Workers, Pid) of
        [{Pid, Ref}] ->
            lager:error("[ ~p ] Received DOWN from worker ~p with reason ~p", [Name, Pid, Reason]),
            ets:delete(Workers, Pid),
            erlang:demonitor(Ref, [flush]);
        _ ->
            lager:error("[ ~p ] Received DOWN from unknown process: ~p / ~p", [Name, Pid, Reason])
    end,
    ets:delete(Workers, Pid),
    maybe_stop(State);

handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_stop(#s{workers = Workers, name = Name} = State) ->
    case ets:first(Workers) == '$end_of_table' of
        true ->
            lager:info("[ ~p ] All workers have finished", [Name]),
            {stop, normal, State};
        false ->
            {noreply, State}
    end.

maybe_report_error(_, {ok, _}) -> ok;
maybe_report_error(Pid, {error, Reason}) ->
    lager:error("Worker ~p has finished abnormally: ~p", [Pid, Reason]);
maybe_report_error(Pid, {exception, Node, {_C, E, ST}}) ->
    lager:error("Worker ~p on ~p has crashed: ~p~nStacktrace: ~p", [Pid, Node, E, ST]).

