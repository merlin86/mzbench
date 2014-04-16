-module(utility_tests).
-include_lib("eunit/include/eunit.hrl").

choose_test() ->
    ?assertEqual(1, utility:choose([1, 1, 1, 1, 1])),
    ?assertEqual(78, utility:choose([78, 78, 78, 78, 78])),
    ?assertEqual([], utility:choose(2, [])),
    ?assertEqual([], utility:choose(0, [4, 4])),
    ?assertEqual([4, 4], utility:choose(20, [4, 4])),
    ?assertEqual([1, 1], utility:choose(2, [1, 1, 1, 1, 1])),
    ?assertEqual([78, 78, 78], utility:choose(3, [78, 78, 78, 78, 78])).
