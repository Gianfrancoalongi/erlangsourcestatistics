-module(bad_expressions).
-export([f/0]).

f() ->
    Id = l:get_user_id(),
    Users = l:get_users(),
    
    L = case lists:keysearch(Id, 1, Users) of
            {value, User} ->
                Val = l:f(User) + 23,
                lists:keyreplace(Id, 1, Users, Val);
            false ->
                Next = l:get_secondary_id(),
                case lists:keysearch(Next, 1, Users) of
                    {value, User} ->
                        Val = l:f(User) + 23,
                        lists:keyreplace(Next, 1, Users, Val);
                    _ ->
                        undefined
                end
        end,
    M = case l:tt(Id, l:get_media_profile()) of
            {y, [{ok, _}, {mid, R}|_]} ->
                R+2;
            {y, [{mid, R}|_]} ->
                R+25;
            undefined ->
                0
        end,       
    
    NewM = case l:umd_r(Id, l:get_users(), M) of
               {y, NewR} ->
                   NewR;
               {n, [{mid, OldR}|_]} ->
                   OldR;
               undefined ->
                   0
           end,
    
    mnesia:transaction(fun() ->
                               X = lists:nth(M, Users),
                               mnesia:write(Id, lists:keyreplace(Id, 1, M, NewM))
                       end).
