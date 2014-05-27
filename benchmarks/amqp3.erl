[
  {pool, [{size, 8},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"},
    {declare_queue, <<"q3">>},
    {subscribe, <<"q3">>},
    {loop, [{time, {1, min}},
            {rate, {1000, rps}}], [
      {publish, <<"q3">>, <<"hello">>}
    ]},
    {disconnect}
  ]}
].
