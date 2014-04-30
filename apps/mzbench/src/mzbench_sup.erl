-module(mzbench_sup).

-export([start_link/0,
         run/1
        ]).

-behaviour(supervisor).
-export([init/1]).

-include("types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

run(ScriptFileName) ->
    Path = case lists:prefix("/", ScriptFileName) of
               true -> ScriptFileName;
               _    -> "../../" ++ ScriptFileName
           end,
    supervisor:start_child(?MODULE, [Path]).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        {director_sup, {director_sup, start_link, []}, transient, infinity, supervisor, [director_sup]}
    ]}}.

