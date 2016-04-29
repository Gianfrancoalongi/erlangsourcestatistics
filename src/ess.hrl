-record(tree,{type :: file | dir,
              name :: string(),
              value = [] :: [],
              children = [] :: [],
              quality :: float()
             }).

-record(val, {max, min, avg, sum, n}).

