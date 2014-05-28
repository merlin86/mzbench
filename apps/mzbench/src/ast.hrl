-record(operation, {
        name = undefined :: atom(),
        args = [] :: abstract_expr(),
        meta = [] :: meta()
        }).

-record(constant, {
        value = undefined :: term(),
        units = undefined :: atom(),
        meta = [] :: meta()
        }).
