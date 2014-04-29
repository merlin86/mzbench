-module(worker_script_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ast.hrl").

validation_ok_simple_test() ->
    ?assertEqual(check([#operation{name = print, args = ["NaNNaNNaNNaNNaNNaN"]},
                        #operation{name = print, args = ["BATMAN"]}]),
                 ok).

validation_ok_larger_test() ->
    Script = [#operation{name = print, args = ["127.0.0.1"]},
              #operation{name = loop, args = [[#operation{name = time, args=[#constant{value = 1, units = min}]},
                      #operation{name = rate, args = [#constant{value = 1, units = rps}]}],
               [#operation{name = print,
                 args = [#operation{name = choose, args = ["queue1", "queue2", "queue3", "queue4", "queue5"]}]}
              ]]},
              #operation{name = loop, args = [[#operation{name = rate, args =[#constant{value = 10, units = rps}]}],
               [#operation{name = print,
                 args = [#operation{ name = choose, args = ["queue1", "queue2", "queue3", "queue4", "queue5"]}]
              }]]}],
    ?assertEqual(ok,
                 check(Script)).

validation_loop_test() ->
    ?assertEqual({invalid_script, ["Loop must have a spec and a body."]},
                 check([#operation{name = loop, args = [#operation{name = print, args = ["NaN"]}]},
                        #operation{name = print, args = ["BATMAN"]}])).

validation_empty_instruction_test() ->
    ?assertEqual({invalid_script, ["Empty instruction."]},
                 check([#operation{}])).

check(Script) ->
    worker_script_validator:validate_worker_script(Script, dummy_worker).
