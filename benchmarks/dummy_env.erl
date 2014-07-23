[
    {include_resource, text, "benchmarks/text.txt"},
    {pool, [{size, 3},
            {worker_type, dummy_worker}],
        [{loop, [{time, {5, sec}},
                    {rate, {1, rps}}],
                [{print, {env, "FOO"}}]}]},

    {pool, [{size, 60},
            {worker_type, dummy_worker}],
        [{print, {choose, {resource, text}}},
            {print, {env, "BAR"}},
            {print, {env, "QUUX"}}]}
].
