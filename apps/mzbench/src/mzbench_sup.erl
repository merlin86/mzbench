-module(mzbench_sup).

-export([start_link/0,
         run/1,
         run/2,
         run_script/2,
         run_script/3,
         wait_finish/1,
         wait_finish/2
        ]).

-behaviour(supervisor).
-export([init/1]).

-include("types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

run(ScriptFileName) -> run(ScriptFileName, application:get_env(mzbench, nodes)).

run(ScriptFileName, undefined) -> run(ScriptFileName, [node()|nodes()]);
run(ScriptFileName, Nodes) ->
    Path = case lists:prefix("/", ScriptFileName) of
               true -> ScriptFileName;
               _    -> "../../" ++ ScriptFileName
           end,
    {ok, ScriptBody} = file:read_file(Path),
    run_script(ScriptFileName, erlang:binary_to_list(ScriptBody), Nodes).

run_script(ScriptFileName, ScriptBody) ->
    run_script(ScriptFileName, ScriptBody, application:get_env(mzbench, nodes)).
run_script(ScriptFileName, ScriptBody, undefined) ->
    run_script(ScriptFileName, ScriptBody, [node()|nodes()]);
run_script(ScriptFileName, ScriptBody, Nodes) ->
    supervisor:start_child(?MODULE, [ScriptFileName, ScriptBody, Nodes]).

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
        child_spec(mzbench_director_sup, [])
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child_spec(I, Args) ->
    {I, {I, start_link, Args}, transient, infinity, supervisor, [I]}.
