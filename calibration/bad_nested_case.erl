
-module(bad_nested_case).

-export([f/0]).

f() ->
    case l:g() of
        true -> case l:h() of
                    false ->
                        case l:u() of
                            good -> not_good;
                            _ -> good
                        end;
                    true ->
                        false
                end;
        false ->
            hello
    end.

