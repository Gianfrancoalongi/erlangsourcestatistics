
-module(bad_variable_steppings).
-export([f/0]).

f() ->
    A = m:g(1),
    A1 = m:g2(A),
    A2 = m:g3(A1),
    A3 = m:g4(A2),
    A3.




