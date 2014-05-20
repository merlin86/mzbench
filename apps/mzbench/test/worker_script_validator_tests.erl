-module(worker_script_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/ast.hrl").

validation_ok_simple_test() ->
    ?assertEqual(check("[{print, \"NaNNaNNaNNaNNaNNaN\"},
                         {print, \"BATMAN\"}]."),
                 ok).

validation_ok_larger_test() ->
    Script = "[{print, \"127.0.0.1\"},
               {loop, [{time, {1, min}},
                       {rate, {1, rps}}],
                [{print, {choose, \"queue1\", \"queue2\", \"queue3\", \"queue4\"}}]},
               {loop, [{rate, {10, rps}}],
                [{print, {choose, \"queue1\", \"queue2\", \"queue3\"}}]}
              ].",
    ?assertEqual(ok, check(Script)).

validation_loop_test() ->
    ?assertEqual({invalid_script, ["line 3: Loop must have a spec and a body."]},
                 check("[{loop, [{rate, {3, rps}}],
                            [{print, \"NaN\"}]},
                         {loop, [{print, \"BATMAN\"}]}].")).

validation_empty_instruction_test() ->
    ?assertEqual({invalid_script, ["line 2: Empty instruction."]},
                 check("[{print, \"foo\"},
                         {},
                         {print, \"bar\"},
                         {print, \"baz\"}].")).

check(S) ->
    Script = string_to_script(S),
    worker_script_validator:validate_worker_script(Script, dummy_worker).

string_to_script(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    ast:transform(Expr).
