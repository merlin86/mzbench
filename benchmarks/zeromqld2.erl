[{pool, [{size, 1}, %% maximum speed test. 1 thread, 1 minute
         {worker_type, zeromqapi_worker}],
  [{connect, "tcp://127.0.0.1:5123", "tcp://127.0.0.1:5123"},
   {loop, [{time, {1, min}}],
    [{detect, "FOO", "en"}]}]}
].
