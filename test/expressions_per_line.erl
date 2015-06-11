-module(expressions_per_line).
-compile({parse_transform,ess}).
-export([call/0]).

call() ->
    %% ---------------------------
    %% 1 2 3
    A=1,
    B_1=2,B_2=2,
    C_1=3,f:g(),[A|T]=[1,2],
    %% ---------------------------
    %% 1 2 1 1
    case 1 of
        2 ->
            A_2=1,
            B_3=2,B_4=2;
        _ ->
            0
    end,
    %% ---------------------------
    %% 5
    case 2 of X -> 1; 2 -> ok end.

            

