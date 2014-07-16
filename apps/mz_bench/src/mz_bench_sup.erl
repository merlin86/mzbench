-module(mz_bench_sup).

-export([start_link/0,
         is_ready/0,
         connect_nodes/1,
         run/1,
         run/2,
         run_script/2,
         run_script/3,
         wait_finish/1,
         wait_finish/2,
         read_script_silent/1,
         macroexpand/1
        ]).

-behaviour(supervisor).
-export([init/1]).

-include("types.hrl").
-include("ast.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

run(ScriptFileName) -> run(ScriptFileName, application:get_env(mz_bench, nodes)).

run(ScriptFileName, undefined) -> run(ScriptFileName, [node()|nodes()]);
run(ScriptFileName, Nodes) ->
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd("../../"),
    try
        ScriptBody = read_script(ScriptFileName),
        run_script(ScriptFileName, ScriptBody, Nodes)
    after
        ok = file:set_cwd(Cwd)
    end.

run_script(ScriptFileName, ScriptBody) ->
    run_script(ScriptFileName, ScriptBody, application:get_env(mz_bench, nodes)).
run_script(ScriptFileName, ScriptBody, undefined) ->
    run_script(ScriptFileName, ScriptBody, [node()|nodes()]);
run_script(ScriptFileName, ScriptBody, Nodes) ->
    supervisor:start_child(?MODULE, [ScriptFileName, ScriptBody, Nodes]).

macroexpand(Path) ->
    Terms = read_file(Path),
    Ctx = [{file_dir, filename:dirname(Path)}],
    walk(Terms, Ctx, fun(F) -> F end, fun expand_macros/2).

% "no logs" reader (used in escripts)
read_script_silent(Path) ->
    Body = io_lib:format("~p", [macroexpand(Path)]),
    parse_script(Body).

read_script(Path) ->
    try
        read_script_silent(Path)
    catch
        C:{parse_error, {_, Module, ErrorInfo}} = E ->
            ST = erlang:get_stacktrace(),
            lager:error("Parsing script file failed: ~s", [Module:format_error(ErrorInfo)]),
            erlang:raise(C,E,ST);
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Failed to read script: ~p 'cause of ~p~nStacktrace: ~p", [Path, E, ST]),
            erlang:raise(C,E,ST)
    end.

-spec parse_script(string()) -> [script_expr()].
parse_script(Body) ->
    case erl_scan:string(Body) of
        {ok, Ts, _} ->
            case erl_parse:parse_exprs(Ts) of
                {ok, [AST]} ->
                    ast:transform(AST);
                {error, Error} ->
                    erlang:error({parse_error, Error})
            end;
        {error, Error, _} ->
            erlang:error({parse_error, Error})
    end.

is_ready() ->
    try
        Apps = application:which_applications(),
        false =/= lists:keyfind(mz_bench, 1, Apps)
    catch
        _:Error ->
            lager:error("is_ready exception: ~p~nStacktrace: ~p", [Error, erlang:get_stacktrace()]),
            false
    end.

connect_nodes(Nodes) ->
    lists:filter(
        fun (N) ->
            pong == net_adm:ping(N)
        end, Nodes).

wait_finish(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.

wait_finish(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after Timeout ->
        {error, Timeout}
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        child_spec(mz_bench_director_sup, [])
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(I, Args) ->
    {I, {I, start_link, Args}, transient, infinity, supervisor, [I]}.

walk(Form, Ctx, Pre, Post) ->
    Form1 = Pre(Form, Ctx),
    Form2 = case Form1 of
        _ when is_list(Form1) ->
            lists:map(fun(F) -> walk(F, Ctx, Pre, Post) end, Form1);
        _ when is_tuple(Form1) ->
            map_tuple(fun(F) -> walk(F, Ctx, Pre, Post) end, Form1);
        _ ->
            Form1
    end,
    Post(Form2, Ctx).

expand_macros({include_resource, N, F}, Ctx) ->
    {resource, N, read_file(proplists:get_value(file_dir, Ctx) ++ "/" ++ F)};
expand_macros({env, N}, _Ctx) ->
    os:getenv(N);
expand_macros(F, _Ctx) -> F.

map_tuple(F, T) ->
    list_to_tuple(lists:map(F, tuple_to_list(T))).

read_file(File) ->
    {ok, [Content]} = file:consult(File),
    Content.
