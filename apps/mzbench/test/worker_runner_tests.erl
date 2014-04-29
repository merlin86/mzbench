-module(worker_runner_tests).

-include_lib("eunit/include/eunit.hrl").

empty_script_test() ->
    ?assertEqual({ok, ""}, run([])).

oneliner_test() ->
    Script = [{print, "Hello", []}],
    ?assertEqual({ok, "Hello"}, run(Script)).

print_three_test() ->
    Script = [{print, "FOO", []}, {print, "BAR", []}, {print, "BAZ", []}],
    ?assertEqual({ok, "FOOBARBAZ"}, run(Script)).

print_loop_test() ->
    Script = [{loop, [{time, {1100, ms}, []},
                      {rate, {4, rps}, []}],
               [{print, "F", []}], []}],
    ?assertEqual({ok, "FFFFF"}, run(Script)).

empty_loop_test() ->
    Script = [{loop, [{time, {0, ms}, []},
                      {rate, {4, rps}, []}],
               [{print, "FOO", []}], []}],
    ?assertEqual({ok, ""}, run(Script)).

empty_loop2_test() ->
    Script = [{loop, [{time, {1100, ms}, []},
                      {rate, {4, rps}, []}],
               [], []}],
    ?assertEqual({ok, ""}, run(Script)).

empty_loop3_test() ->
    Script = [{loop, [{time, {1100, ms}, []},
                      {rate, {0, rps}, []}],
               [{print, "FOO", []}], []}],
    ?assertEqual({ok, ""}, run(Script)).

run(Script) ->
    meck:new(folsom_metrics), % because folsom_metrics requires started folsom app
    meck:expect(folsom_metrics, notify, fun(_,_,_)->ok end),
    R = worker_runner:run_worker_script(0, Script, dummy_worker),
    meck:unload(folsom_metrics),
    R.
