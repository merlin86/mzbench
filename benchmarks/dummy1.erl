[{pool, [{size, 3},
         {worker_type, dummy_worker}],
  [{loop, [{time, {5, sec}},
           {rate, {1, rps}}],
    [{print, "FOO"}]}]},

 {pool, [{size, 2},
         {worker_type, dummy_worker}],
  [{print, "OHAI"},
   {print, "BAR"},
   {print, "QUUX"}]}
].
