-module(mzbench_sup).
-behaviour(supervisor).

-export([start_link/0, run/1]).
-export([init/1]).

-include("types.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, infinity, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        ?CHILD(director_sup, supervisor)
    ]}}.

run(ScriptFileName) ->
    Path = case lists:prefix("/", ScriptFileName) of
               true -> ScriptFileName;
               _    -> "../../" ++ ScriptFileName
           end,
    supervisor:start_child(?MODULE, [Path]).
