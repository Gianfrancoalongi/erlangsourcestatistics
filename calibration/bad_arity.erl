-module(bad_arity).
-export([f/0]).

f() ->
    A = 1,
    B = [1,2,3],
    C = {ok,2},
    D = undefined,
    E = undefined,
    F = 2,
    g(A,B,C,D,E,F).

g(A,B,C,D,E,F) when F > 3 ->
    case C of
        {ok, F} ->
            h(A,C,D,B,E,F);
        _ ->
            g(A,B,C,D,E,F-1)
    end;
g(A,B,C,D,E,F) when length(B) < 4 ->
    case B of
        [1,2,3] ->
            h(A,C,D,B,E,F);
        [] ->
            i(A,C,B,D,F,E)
    end;
g(A,B,C,D,E,F) ->
    NewB = l(A,B,C,D,E,F),
    g(A,NewB,C,D,E,F).

        
h(A,C,D,B,E,F) ->
    g(A,B,C,D,F,E).

i(A,C,B,D,F,E) ->
    h(A,B,C,D,E,F).

l(_,B,_,_,_,_) ->
    tl(B).
