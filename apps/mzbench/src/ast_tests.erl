-module(ast_tests).
-include_lib("eunit/include/eunit.hrl").

markup_test() ->
  ?assertEqual({cons,1,{nil,2}}, ast:markup({cons,1,{nil,2}})).

markup2_test() ->
  Input = {cons,1,
            {tuple,1,[{atom,1,size},
                      {integer,1,3}]},
            {cons,2,
             {tuple,2,[{atom,2,worker_type},
                       {atom,2,dummy_worker}]},
             {nil,2}}},
  Output = {cons,1,
            {tuple,1,[{atom,1,size},
                      {integer,1,3},
                      {cons, 1, {tuple, 1, [{atom, 1, line}, {integer, 1, 1}]}, {nil, 1}}]},
            {cons,2,
             {tuple,2,[{atom,2,worker_type},
                       {atom,2,dummy_worker},
                       {cons, 2, {tuple, 2, [{atom, 2, line}, {integer, 2, 2}]}, {nil, 2}}]},
             {nil,2}}},
  ?assertEqual(Output, ast:markup(Input)).

