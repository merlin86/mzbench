
-type worker_state() :: term().
-type script_value() :: term().
-type script_expr() :: tuple() | script_value().
-type script_loopspec() :: [tuple()].
-type script_validation_result() :: ok | {invalid_script, [string()]}.
-type pool() :: {[script_expr()], module(), integer()}.
-type named_pool() :: {atom(), [script_expr()], module(), integer()}.

