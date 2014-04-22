-module(mzbench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Args = init:get_plain_arguments(),
    case Args of
        ["console", ScriptFileName] ->
            mzbench_sup:start_link(ScriptFileName);
        _ ->
            lager:error(" Usage: ERL_FLAGS=path-to-script make run
                          Path is relative to rel/mzbench/bin")
    end.

stop(_State) ->
    ok.
