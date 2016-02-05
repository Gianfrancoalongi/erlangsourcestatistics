-record(tree,{type :: file | dir,
              name :: string(),
              value :: [],
              children = [] :: []
             }).
