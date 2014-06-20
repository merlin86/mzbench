
# mzbench

Distributed load testing tool.

Work in progress.

# Usage

````
make run benchmarks/dummy1.erl
```

or

```
make run
...
(erlsh)> mzbench_sup:run("benchmarks/dummy1.erl").
```

## Configuring statistics export to graphite

You can customize graphite address and port in mzbench section of
`rel/files/sys.config` or in `rel/mzbench/releases/<release-name>/sys.config`:

```
  ...
  {mzbench, [{graphite_host, "localhost"},
             {graphite_port, 2003}]}
  ...
```

If you don't have graphite, just remove these tuples from config.

# Scripting language

Any script should be valid erlang list of tuples:

```
[{tuple ...},
{tuple ...},
...
{tuple ...}].
```

The only supported type of tuple at top level is pool:
```
{pool, OptionsList, OperationsList}
```

Options are:
```
{size, N} -- number of workers to create
{sample_metrics, P} -- proportion of metrics to be gathered, P is between 0 (no metrics) and 1 (all)
{worker_type, Atom} -- amqp_worker or dummy_worker currently
```

Following operations are built-in (supported for any worker_type):
```
{random_bytes, SizeConstant} -- generate sequence of random bytes with specific length
{choose, List} -- choose random element from List
{choose, N, List} -- choose N random elements from List
{loop, LoopOptions, OperationsList} -- execute OperationsList with specific options
```

LoopOptions are:
```
{rate, RateConstant} -- maximum number of executions for OptionsList per time period
{time, TimeConstant} -- execution time
```

RateConstants supported:
```
{N, rps} {N, rpm} {N, rph}
```

TimeConstants supported:
```
{N, sec} {N, min} {N, ms} {N, h}
```

SizeConstants supported:
```
{N, b}, {N, kb}, {N, mb}, {N, gb}, {N, tb}
```

Operations for dummy_worker:
```
{print, String} -- put String into worker state
```

dummy_worker script example:
```
[{pool, [{size, 3}, %% three equal workers will execute the same body
         {worker_type, dummy_worker}],
  [{loop, [{time, {5, sec}}, %% loop with time and rate limit
           {rate, {1, rps}}],
    [{print, "FOO"}]}]}, %% dummy_worker operation call

 {pool, [{size, 2}, %% another pool
         {worker_type, dummy_worker}],
  [{print, "OHAI"}, %% sequence of dummy_worker operations
   {print, "BAR"},
   {print, "QUUX"}]}
].
```

Operations for amqp_worker:
```
{connect, String} -- connect amqp server, example: {connect, "amqp://127.0.0.1:5672"}
{disconnect}
{declare_exchange, BinaryName}
{declare_queue, BinaryName}
{bind, ExchangeName, RoutingKey, InQueue} -- all arguments for amqp functions below are binary
{publish, BinaryKey, Message}
{publish, ExchangeName, BinaryKey, Message}
{get, InQueue}
{subscribe, InQueue}
```

amqp_worker script example:
```
[
  {pool, [{size, 3},
          {worker_type, amqp_worker}], [
    {connect, "amqp://127.0.0.1:5672"}, %% amqp server address
    {declare_queue, <<"q3">>}, %% queue name
    {subscribe, <<"q3">>},
    {loop, [{time, {1, min}}, %% this loop will try execute its body ({publish,...} tuple) about 6000 times in a minute
            {rate, {100, rps}}], [
      {publish, <<"q3">>, <<"hello">>} %% worker operation call
    ]},
    {disconnect} %% close connection to amqp
  ]}
].
```

# Syntax definition

```
| -- or
A -- Erlang Atom
N -- Integer
P -- Float between 0 and 1
Script = PoolList.
XList = [] | [X XList]
Pool = {pool, OptionList, OperationList}
Option = {size, N} | {sample_metrics, P} | {worker_type, A}
Operation = {OperationName} | {OperationName, Arguments}
Arguments = Argument, Arguments | Argument
Argument = Constant | Operation
Constant = {N, S}
```

## How to add a new worker

Workers are implemented as erlang modules that export `initial_state/0` and
some worker-specific functions.

```
-type state() :: term().
-type result() :: term().

-spec initial_state() -> state().
```
Used by worker script interpreter to initialize worker-specific state

Any worker-specific function `foo/N` must have the following type:
```
-spec foo(state(), ... N - 1 terms ...) -> {result(), state()}.
```
foo here accepts worker state and N - 1 parameters and returns a result and
a new worker state.

Functions that are used solely to modify worker state (e.g. logging function)
return `{nil, NewState}`.

See [dummy_worker](apps/mzbench/src/dummy_worker.erl) module as an example.

# Packaging

`make rpm` builds an rpm using fpm.

