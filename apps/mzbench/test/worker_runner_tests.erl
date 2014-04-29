-module(worker_runner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ast.hrl").

empty_script_test() ->
    ?assertEqual({ok, ""}, run([])).

oneliner_test() ->
    Script = [#operation{name = print, args = ["Hello"]}],
    ?assertEqual({ok, "Hello"}, run(Script)).

print_three_test() ->
    Script = [#operation{name = print, args = ["FOO"]},
              #operation{name = print, args = ["BAR"]},
              #operation{name = print, args = ["BAZ"]}],
    ?assertEqual({ok, "FOOBARBAZ"}, run(Script)).

print_loop_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
               [#operation{name = print, args = ["F"]}]]}],
    ?assertEqual({ok, "FFFFF"}, run(Script)).

empty_loop_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 0, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
               [#operation{name = print, args = ["FOO"]}]]}],
    ?assertEqual({ok, ""}, run(Script)).

empty_loop2_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
                 []]}],
    ?assertEqual({ok, ""}, run(Script)).

empty_loop3_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 0, units = rps}]}],
               [#operation{name = print, args = "FOO"}]]}],
    ?assertEqual({ok, ""}, run(Script)).

run(Script) ->
    meck:new(folsom_metrics), % because folsom_metrics requires started folsom app
    meck:expect(folsom_metrics, notify, fun(_,_,_)->ok end),
    R = worker_runner:run_worker_script(0, Script, dummy_worker),
    meck:unload(folsom_metrics),
    R.
