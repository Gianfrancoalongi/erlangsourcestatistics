
-module(good_variable_steppings).
-export([f/0]).

f() ->
    s:seq(1, [fun m:g/1,
              fun m:g2/1,
              fun m:g3/1,
              fun m:g4/1]).

