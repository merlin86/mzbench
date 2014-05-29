-module(mzbench_director_sup).
-export([start_link/2,
         start_child/3,
         stop/1,
         get_graphite_client/1
        ]).

-behaviour(supervisor).
-export([init/1]).

-include("types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(ScriptFileName, Nodes) ->
    lager:info("[ director_sup ] Loading ~p", [ScriptFileName]),
    RunId = make_run_id(ScriptFileName),
    case read_script(ScriptFileName, RunId) of
        {ok, Script} ->
            supervisor:start_link(?MODULE, [Script, Nodes, RunId]);
        {error, E} ->
            {error, {shutdown, E}}
    end.

start_child(Pid, Module, Args) ->
    supervisor:start_child(Pid, {make_ref(), {Module, start_link, Args}, temporary, 1000, worker, [Module]}).

stop(Pid) ->
    {ok, MetricsPid} = get_child_pid(Pid, mzbench_metrics),
    mzbench_metrics:trigger(MetricsPid),
    supervisor:terminate_child(mzbench_sup, Pid).

get_graphite_client(Self) ->
    case get_child_pid(Self, graphite_client_sup) of
        no_such_child -> noclient;
        {ok, GraphiteSupPid} -> graphite_client_sup:get_client(GraphiteSupPid)
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([Script, Nodes, RunId]) ->
    lager:info("[ director_sup ] I'm at ~p", [self()]),
    {ok, {{one_for_one, 5, 1}, [
        child_spec(worker, mzbench_director, temporary, [self(), Script, Nodes]),
        child_spec(supervisor, graphite_client_sup, transient, []),
        child_spec(worker, mzbench_metrics, transient, [metrics_prefix(RunId), Nodes, self()])
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(WorkerOrSupervisor, I, Restart, Args) ->
    {I, {I, start_link, Args}, Restart, 1000, WorkerOrSupervisor, [I]}.

-spec read_script(string(), string()) -> {ok, [script_expr()]} | {error, any()}.
read_script(ScriptFileName, RunId) ->
    {ok, Contents} = file:read_file(ScriptFileName),
    case erl_scan:string(binary_to_list(Contents)) of
        {ok, Ts, _} ->
            {ok, [AST]} = erl_parse:parse_exprs(Ts),
            Script = ast:transform(AST),
            Script2 = ast:map_meta(
                fun (M, Op) ->
                    Line  = proplists:get_value(line, M),
                    P = lists:flatten(io_lib:format("~s/POOL/~p-~s", [metrics_prefix(RunId), Line, Op])),
                    [{metric_prefix, P}|M]
                end, Script),
            FinalScript = ast:add_meta(Script2, [{run_id, RunId}]),
            {ok, FinalScript};
        {error, {_, erl_parse, E}, _} ->
            lager:error("Parsing script file failed: ~p", [E]),
            {error, E};
        A ->
            lager:error("Reading script file failed: ~p", [A]),
            {error, A}
    end.

-spec make_run_id(string()) -> string().
make_run_id(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    lists:flatten(io_lib:format("~s-~s", [Name, iso_8601_fmt(erlang:localtime())])).

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).

metrics_prefix(RunId) ->
    "mzbench/" ++ RunId.

get_child_pid(SelfPid, ChildId) ->
    Children = supervisor:which_children(SelfPid),
    case lists:keyfind(ChildId, 1, Children) of
        false -> no_such_child;
        ChildTuple -> {ok, element(2, ChildTuple)}
    end.

