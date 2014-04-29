-record(operation, {
        name = undefined,
        args = [],
        meta = []
        }).

-record(constant, {
        value = undefined,
        units = undefined,
        meta = []
        }).

-record(pool, {
        name = undefined,
        opts = [],
        script = undefined,
        meta = []
        }).
