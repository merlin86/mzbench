
# mzbench

Distributed load testing tool.

Work in progress.

## How to add a new worker

Workers are implemented as erlang modules that export `initial_state/0` and
some worker-specific functions.

```
-type state() :: term().
-type result() :: term().

-spec initial_state() -> state().
```
Used by worker script interpreter to initialize worker-specific state

Any worker-specific function foo/N must have the following type:
```
-spec foo(state(), ... N - 1 terms ...) -> {result(), state()}.
```
foo here accepts worker state and N - 1 parameters and returns a result and
a new worker state.

Functions that are used solely to modify worker state (e.g. logging function)
return `{nil, NewState}`.

See [dummy_worker](apps/mzbench/src/dummy_worker.erl) module as an example.
