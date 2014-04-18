-module(worker_runner_tests).

-include_lib("eunit/include/eunit.hrl").

empty_script_test() ->
    ?assertEqual({ok, ""}, run([])).

oneliner_test() ->
    Script = [{print, "Hello"}],
    ?assertEqual({ok, "Hello"}, run(Script)).

print_three_test() ->
    Script = [{print, "FOO"}, {print, "BAR"}, {print, "BAZ"}],
    ?assertEqual({ok, "FOOBARBAZ"}, run(Script)).

run(Script) ->
    worker_runner:run_worker_script(0, Script, dummy_worker).
