-record(tree,{type :: file | dir,
              name :: string(),
              value :: [],
              children = [] :: []
             }).

-record(val, {max, min, avg, sum, n}).

