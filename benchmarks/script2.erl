[{pool, [{size, 3},
         {worker_type, dummy_worker}],
  [{loop, [{time, {1, min}},
           {rate, {1, rps}}],
    [{print, "FOO"}]}]},

 {pool, [{size, 2},
         {worker_type, dummy_worker}],
  [{print, "OHAI"},
   {print, "BAR"},
   {print, "QUUX"}]}
].
