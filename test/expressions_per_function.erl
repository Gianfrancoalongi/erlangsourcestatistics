-module(expressions_per_function).
-compile({parse_transform,ess}).
-export([a/1]).

a(N) when is_integer(N) ->
    case N of
        2 ->
            something_else,
            case N+1 of
                3 ->
                    what_about,
                    this_line,
                    m:f("one");
                1 ->
                    ok
            end,
            something;
        _ ->
            ignore,
            m:f(),
            b()
    end;
a(X) when is_list(X) ->
   X;
a(_) ->
   k.

b() ->
    this_one.




