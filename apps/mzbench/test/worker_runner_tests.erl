-module(worker_runner_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/types.hrl").
-include("../src/ast.hrl").

empty_script_test() ->
    ?assertEqual("", run([])).

oneliner_test() ->
    Script = [#operation{name = print, args = ["Hello"]}],
    ?assertEqual("Hello", run(Script)).

print_three_test() ->
    Script = [#operation{name = print, args = ["FOO"]},
              #operation{name = print, args = ["BAR"]},
              #operation{name = print, args = ["BAZ"]}],
    ?assertEqual("FOOBARBAZ", run(Script)).

print_loop_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
               [#operation{name = print, args = ["F"]}]]}],
    ?assertEqual("FFFFF", run(Script)).

empty_loop_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 0, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
               [#operation{name = print, args = ["FOO"]}]]}],
    ?assertEqual("", run(Script)).

empty_loop2_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 4, units = rps}]}],
                 []]}],
    ?assertEqual("", run(Script)).

empty_loop3_test() ->
    Script = [#operation{name = loop, args = [[#operation{name = time, args = [#constant{value = 1100, units = ms}]},
                      #operation{name = rate, args = [#constant{value = 0, units = rps}]}],
               [#operation{name = print, args = "FOO"}]]}],
    ?assertEqual("", run(Script)).

run(Script) ->
    meck:new(folsom_metrics), % because folsom_metrics requires started folsom app
    meck:expect(folsom_metrics, notify, fun(_,_,_)->ok end),
    meck:expect(folsom_metrics, safely_notify, fun(_,_,_)->ok end),
    {_, R} = worker_runner:eval_expr(Script, dummy_worker:initial_state(), dummy_worker),
    meck:unload(folsom_metrics),
    R.
