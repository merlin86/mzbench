-module(mzbench_pool).

-behaviour(gen_server).

%% API
-export([start_link/3,
         stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
        workers = [],
        name = undefined
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, PoolOpts, Script) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, PoolOpts, Script], []).

stop(Name) ->
    gen_server:call(Name, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, PoolOpts, Script]) ->
    lager:info("Pool ~p: Starting...", [Name]),
    Size = proplists:get_value(size, PoolOpts, undefined),
    WorkerModule = proplists:get_value(worker, PoolOpts, undefined),
    WorkerOpts = proplists:get_value(worker_opts, PoolOpts, undefined),
    Workers = start_workers(Size, WorkerOpts, Script, WorkerModule, []),
    lager:info("Pool ~p: started (~p workers)", [Name, Size]),
    {ok, #s{name = Name, workers = Workers}}.

handle_call(stop, _From, State = #s{workers = Workers, name = Name}) ->
    lager:info("Pool ~p: received stop signal", [Name]),
    lists:foreach(
        fun ({Pid, Ref}) ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit(Pid, kill)
        end, Workers),
    {stop, normal, ok, State = #s{workers = []}};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_call, Msg}, State}.

handle_info({'DOWN', Ref, _, Pid, _Reason}, State = #s{workers = Workers, name = Name}) ->
    case lists:delete({Pid, Ref}, Workers) of
        [] ->
            lager:info("Pool ~p: All workers has finished", [Name]),
            {stop, normal, State#s{workers = []}};
        NewWorkers ->
            {noreply, State#s{workers = NewWorkers}}
    end;

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

start_workers(N, WorkerOpts, Script, WorkerModule, Res) when N > 0, is_integer(N) ->
    {ok, P} = mzbench_worker_sup:start_worker(WorkerOpts, Script, WorkerModule),
    Ref = erlang:monitor(process, P),
    start_workers(N - 1, WorkerOpts, Script, WorkerModule, [{P, Ref}|Res]);
start_workers(_, _, _, _, Res) -> Res.

