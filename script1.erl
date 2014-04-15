[
  {pool, [ { size, 5000 },
           { time, { 10, min } },
           { worker_type, rmq_worker } ],
    [
      { connect, [{hosts, ["127.0.0.1"]}] },
      { loop, [{ time, { 1, min } },
               { rate, { 1, rps } }],
        [ { publish, 
            { choose, ["queue1", "queue2", "queue3", "queue4", "queue5"] },
            { random_bytes, { 2, kb } }} ]
      },
      { loop, [{ rate, { 10, rps } }],
        [ { publish, 
            { choose, ["queue1", "queue2", "queue3", "queue4", "queue5"] },
            { random_bytes, 500 }} ]
      }
    ]}
].
