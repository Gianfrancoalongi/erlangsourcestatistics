-record(tree,{type :: file | dir,
              name :: string(),
              value :: [],
              children = [] :: []
             }).

-record(agg, {max, min, avg, sum, n}).

