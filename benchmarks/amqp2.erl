[
  {pool, [{size, 3},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"},
    {declare_exchange, <<"x2">>},
    {loop, [{time, {1, min}},
            {rate, {100, rps}}], [
      {publish, <<"x2">>, <<"key">>, <<"hello">>}
    ]},
    {disconnect}
  ]},
  {pool, [{size, 3},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"},
    {declare_exchange, <<"x2">>},
    {declare_queue, <<"q2">>},
    {bind, <<"x2">>, <<"key">>, <<"q2">>},
    {loop, [{time, {1, min}}], [
      {get, <<"q2">>}
    ]},
    {disconnect}
  ]}
].
