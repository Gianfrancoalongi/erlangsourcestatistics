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


-record(hist,{base_element = 0,
              in_block = false,
              'case' = 0,
              'match' = 0,
              'lhs' = 0,
              'rhs' = 0,
              'call' = 0,
              'try' = 0,
              'catch' = 0,
              'function' = 0,
              'clause' = 0,
              'clauses' = 0,
              'bin' = 0,
              'bin_element' = 0,
              'if' = 0,
              'receive' = 0,
              'cons' = 0,
              'record' = 0,
              'record_field' = 0,
              'record_index' = 0,
              'tuple' = 0,
              'op' = 0,
              'lc' = 0,
              'generate' = 0,
              'b_generate' = 0,
              'fun' = 0,
              'block' = 0,
              'bc' = 0
             }).


