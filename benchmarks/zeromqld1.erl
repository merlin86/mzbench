[{pool, [{size, 1},
         {worker_type, zeromqld2_worker}],
  [{connect, "tcp://127.0.0.1:5123", "tcp://127.0.0.1:5123"},
   {loop, [{time, {5, sec}},
           {rate, {1000, rps}}],
    [{detect, "FOO", "en"}]}]}
].
