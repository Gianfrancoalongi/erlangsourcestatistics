
-module(good_nested_case).

-export([f/0]).

f() ->
    case l:g() of
        true -> 
            do_h();
        false ->
            hello
    end.

do_h() ->
    case l:h() of
        false ->
            do_u();
        true ->
            false
    end.

do_u() ->
    case l:u() of
        good -> not_good;
        _ -> good
    end.

