-record(tree,{type :: file | dir | function,
              name :: string(),
              raw_values :: [],
              statistics :: [],
              quality_penalty :: [],
              children = [] :: [],
              quality = 100 :: float(),
              value
             }).

-record(val, {max, min, avg, sum, n}).

