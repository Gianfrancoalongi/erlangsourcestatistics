-module(good_clauses).
-export([f/0]).

f() ->
    P = [1,a,2,{ok,1},b,3,{error,2},c, -2, -10, 20],
    { ints(P), atoms(P), tuples(P) }.

ints(P) ->
    [ I || I <- P, is_integer(I) ].

atoms(P) ->
    [ I || I <- P, is_atom(I) ].

tuples(P) ->
    [ I || I <- P, is_tuple(I) ].

