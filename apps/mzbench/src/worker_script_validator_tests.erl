-module(worker_script_validator_tests).

-include_lib("eunit/include/eunit.hrl").

validation_ok_simple_test() ->
    ?assertEqual(check([{print, "NaNNaNNaNNaNNaNNaN"}, {print, "BATMAN"}]),
                 ok).

validation_ok_larger_test() ->
    Script = [{print, "127.0.0.1"},
              {loop, [{time, {1, min}},
                      {rate, {1, rps}}],
               [{print,
                 {choose, ["queue1", "queue2", "queue3", "queue4", "queue5"]}}]
              },
              {loop, [{rate, {10, rps}}],
               [{print,
                 {choose, ["queue1", "queue2", "queue3", "queue4", "queue5"]}}]
              }],
    ?assertEqual(ok,
                 check(Script)).

validation_loop_test() ->
    ?assertEqual({invalid_script, ["Loop must have a spec and a body."]},
                 check([{loop, [{print, "NaN"}]}, {print, "BATMAN"}])).

validation_empty_instruction_test() ->
    ?assertEqual({invalid_script, ["Empty instruction."]},
                 check([{}])).

check(Script) ->
    worker_script_validator:validate_worker_script(Script, dummy_worker).
