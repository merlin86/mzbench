-module(mzbench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _) ->
    % TODO: unhardcode
    mzbench_sup:start_link("../../benchmarks/script1.erl");
start(_, Args) ->
    lager:error("Args: ~p~n", [Args]),
    lager:error("Usage: make run ./path-to-script"),
    shutdown.

stop(_State) ->
    ok.
