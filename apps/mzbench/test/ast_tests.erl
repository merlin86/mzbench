-module(ast_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/ast.hrl").

transform_test() ->
  ?assertEqual([], ast:transform({nil,1})).

transform2_test() ->
  Input = {cons,1,
            {tuple,1,[{atom,1,size},
                      {integer,1,3}]},
            {cons,2,
             {tuple,2,[{atom,2,worker_type},
                       {atom,2,dummy_worker}]},
             {nil,2}}},
  Output = [#operation{name = size, args = [3], meta = [{line, 1}]},
            #operation{name = worker_type, args = [dummy_worker], meta = [{line, 2}]}],
  ?assertEqual(Output, ast:transform(Input)).

