#!/usr/bin/env escript
-mode(compile).

main(Path) ->
    CodePaths = [filename:join(filename:dirname(escript:script_name()), "./apps/mz_bench/ebin/") |
                 filelib:wildcard("/mz/mz_bench/lib/mz_bench-*/ebin/")],
    code:add_pathsz(CodePaths),
    Terms = mz_bench_sup:macroexpand(Path),
    io:format("~p", [Terms]).
  
