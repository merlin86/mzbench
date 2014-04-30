-module(worker_pool).

-export([start_link/2,
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

start_link(SuperPid, Pool) ->
    gen_server:start_link(?MODULE, [SuperPid, Pool], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, Pool]) ->
    gen_server:cast(self(), {start_workers, SuperPid, Pool}),
    {ok, #s{}}.

handle_call(stop, _From, State = #s{workers = Workers, name = Name}) ->
    lager:info("[ ~p ] Received stop signal", [Name]),
    lists:foreach(
        fun ({Pid, Ref}) ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit(Pid, kill)
        end, Workers),
    {stop, normal, ok, State = #s{workers = []}};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_workers, SuperPid, Pool}, State) ->
    #operation{name = pool, args = [PoolOpts, Script], meta = Meta} = Pool,
    Name = proplists:get_value(pool_name, Meta),
    [Size] = mproplists:get_value(size, PoolOpts, [undefined]),
    [WorkerModule] = mproplists:get_value(worker_type, PoolOpts, [undefined]),
    [WorkerOpts] = mproplists:get_value(worker_opts, PoolOpts, [undefined]),
    Workers = start_workers(SuperPid, Size, [WorkerOpts, Script, WorkerModule], []),
    lager:info("[ ~p ] Started ~p workers", [Name, Size]),
    {noreply, State#s{
        name    = Name,
        workers = Workers
    }};
handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info({'DOWN', Ref, _, Pid, _Reason}, State = #s{workers = Workers, name = Name}) ->
    case lists:delete({Pid, Ref}, Workers) of
        [] ->
            lager:info("[ ~p ] All workers have finished", [Name]),
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

start_workers(SuperPid, N, Args, Res) when N > 0, is_integer(N) ->
    {ok, P} = director_sup:start_child(SuperPid, worker_runner, Args),
    Ref = erlang:monitor(process, P),
    start_workers(SuperPid, N-1, Args, [{P, Ref} | Res]);
start_workers(_, _, _, Res) -> Res.

