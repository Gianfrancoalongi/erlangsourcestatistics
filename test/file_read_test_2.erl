-module(file_read_test_2).
-export([a/1,b/2,c/3,d/4]).

a(X) when X > 2 ->
    case b(X,X/2) of
        {ok,N} ->
            N;
        {error,Y} ->
            case c(Y,X,X/2) of
                {ok, M} ->
                    M;
                {error, Z} ->
                    d(Z,Z,Z,Z)
            end
    end;
a(X) ->
    X.

b(X,Y) when Y*2 == X ->
    {ok, 3*X + Y};
b(X,Y) when X*2 == Y ->
    {ok, 3*Y - X};
b(X,Y) ->
    {error, X+Y}.

c(_,_, _) -> 4.
d(_,_, _, _) -> 4.
