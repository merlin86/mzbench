-module(mz_bench_director_sup).
-export([start_link/3,
         start_child/3,
         stop/1,
         get_graphite_client/1,
         metrics_prefix/1
        ]).

-behaviour(supervisor).
-export([init/1]).

-include("types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(ScriptFileName, ScriptBody, Nodes) ->
    lager:info("[ director_sup ] Loading ~p", [ScriptFileName]),
    RunId = make_run_id(ScriptFileName),
    supervisor:start_link(?MODULE, [add_runid_meta(ScriptBody, RunId) , Nodes, RunId]).

start_child(Pid, Module, Args) ->
    supervisor:start_child(Pid, {make_ref(), {Module, start_link, Args}, temporary, 1000, worker, [Module]}).

stop(Pid) ->
    {ok, MetricsPid} = get_child_pid(Pid, mz_bench_metrics),
    mz_bench_metrics:trigger(MetricsPid),
    supervisor:terminate_child(mz_bench_sup, Pid).

get_graphite_client(Self) ->
    case get_child_pid(Self, graphite_client_sup) of
        {error, no_such_child}-> noclient;
        {ok, GraphiteSupPid} -> graphite_client_sup:get_client(GraphiteSupPid)
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([Script, Nodes, RunId]) ->
    lager:info("[ director_sup ] I'm at ~p", [self()]),
    {ok, {{one_for_one, 5, 1}, [
        child_spec(worker, mz_bench_director, temporary, [self(), Script, Nodes]),
        child_spec(supervisor, graphite_client_sup, transient, []),
        child_spec(worker, mz_bench_metrics, transient, [metrics_prefix(RunId), Nodes, self()])
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(WorkerOrSupervisor, I, Restart, Args) ->
    {I, {I, start_link, Args}, Restart, 1000, WorkerOrSupervisor, [I]}.


-spec make_run_id(string()) -> string().
make_run_id(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    lists:flatten(io_lib:format("~s-~s", [Name, iso_8601_fmt(erlang:localtime())])).

add_runid_meta(Script, RunId) ->
    Script2 = ast:map_meta(
        fun (M, Op) ->
            Line  = proplists:get_value(line, M),
            P = lists:flatten(io_lib:format("~s/POOL/~p-~s", [mz_bench_director_sup:metrics_prefix(RunId), Line, Op])),
            [{metric_prefix, P}|M]
        end, Script),
    ast:add_meta(Script2, [{run_id, RunId}]).

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).

metrics_prefix(RunId) ->
    "mz_bench/" ++ RunId.

-spec get_child_pid(pid(), ChildId :: atom())
    -> {ok, pid()} | {error, Reason :: term()}.
get_child_pid(SelfPid, ChildId) ->
    Children = supervisor:which_children(SelfPid),
    case lists:keyfind(ChildId, 1, Children) of
        false -> {error, no_such_child};
        {_, P, _, _} -> {ok, P}
    end.

