-module(mzbench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    Args = init:get_plain_arguments(),
    case Args of
        ["console", []] ->
            %% This is what happens when 'mzbench start' is called.
            mzbench_sup:start_link();
        ["console", Script] ->
            Sup = mzbench_sup:start_link(),
            mzbench_sup:run(Script),
            Sup;
        _ ->
           mzbench_sup:start_link()
    end.

stop(_State) ->
    ok.
