-module(function_clauses_per_function).
-compile({parse_transform,ess}).
-export([a/3,
         b/2,
         c/1]).

a(1,1,1) ->
    1;
a(2,2,2) ->
    2;
a(3,3,3) ->
    3;
a(4,4,4) ->
    4;
a(5,5,5) ->
    5;
a(6,6,6) ->
    6.

b(1,1) ->
    1;
b(2,2) ->
    2;
b(3,3) ->
    3;
b(4,4) ->
    4.


c(1) ->
    1;
c(2) ->
    2.








