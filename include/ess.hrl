-record(tree,{type :: file | dir | function,
              id :: integer(),
              child_of = [],
              name :: string(),
              raw_values :: [],
              statistics :: [],
              quality_penalty :: [],
              children = [] :: [],
              quality = 100 :: float(),
              value,
              collapsed = false,
              render = false
             }).

-record(val, {max, min, avg, sum, n}).

