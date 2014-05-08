[
  {pool, [{size, 3},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"},
    {declare_queue, <<"q1">>},
    {loop, [{time, {1, min}},
            {rate, {10, rps}}], [
      {publish, <<"q1">>, <<"hello">>}
    ]},
    {disconnect}
  ]},
  {pool, [{size, 3},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"},
    {declare_queue, <<"q1">>},
    {loop, [{time, {1, min}}], [
      {get, <<"q1">>}
    ]},
    {disconnect}
  ]}
].
