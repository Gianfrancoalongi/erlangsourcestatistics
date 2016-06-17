-module(good_expressions).
-export([f/0]).

f() ->
    Id = m:get_user_id(),
    Users = m:get_users(),
    L = update_user_list(Id, Users),
    M = default_media_profile(Id),
    NewM = update_media_profile(Id, M), 
    store_user(Id, Users, M, NewM).

update_user_list(Id, Users) ->
    case do_update_user_list(Id, Users) of
        undefined ->
            Next = m:get_secondary_id(),
            do_update_user_list(Next, Users);
        Res ->
            Res
    end.

do_update_user_list(Id, Users) ->
    case lists:keysearch(Id, 1, Users) of
        {value, User} ->
            Val = m:f(User) + 23,
            lists:keyreplace(Id, 1, Users, Val);
        _ ->
            undefined
    end.

default_media_profile(Id) ->
    case m:tt(Id, m:get_media_profile()) of
        {y, [{ok, _}, {mid, R}|_]} ->
            R+2;
        {y, [{mid, R}|_]} ->
            R+25;
        undefined ->
            0
    end.

update_media_profile(Id, M) ->
    case m:umd_r(Id, m:get_users(), M) of
        {y, NewR} ->
            NewR;
        {n, [{mid, OldR}|_]} ->
            OldR;
        undefined ->
            0
    end.

store_user(Id, Users, M, NewM) ->    
    mnesia:transaction(fun() ->
                               X = lists:nth(M, Users),
                               mnesia:write(Id, lists:keyreplace(Id, 1, M, NewM))
                       end).

