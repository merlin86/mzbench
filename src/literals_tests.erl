-module(literals_tests).
-include_lib("eunit/include/eunit.hrl").

bytes_test() ->
    ?assertEqual({921, b}, literals:convert({921, b})),
    ?assertEqual({9216, b}, literals:convert({9, kb})),
    ?assertEqual({7340032, b}, literals:convert({7, mb})),
    ?assertEqual({7516192768, b}, literals:convert({7, gb})),
    ?assertEqual({7696581394432, b}, literals:convert({7, tb})).

seconds_test() ->
    ?assertEqual({3000, ms}, literals:convert({3, s})),
    ?assertEqual({120000, ms}, literals:convert({2, min})),
    ?assertEqual({7200000, ms}, literals:convert({2, h})).

rps_test() ->
    ?assertEqual({12, rps}, literals:convert({12, rps})),
    ?assertEqual({2/60, rps}, literals:convert({2, rpm})),
    ?assertEqual({2/60/60, rps}, literals:convert({2, rph})).

complex_test() ->
    Expected = [{pool, [ { size, {5*1024, b} },
           { time, { 600000, ms } },
           { worker_type, rmq_worker } ]}],
    ?assertEqual(Expected,
                 literals:convert([{pool, [ { size, {5, kb} },
           { time, { 10, min } },
           { worker_type, rmq_worker }]}])).
