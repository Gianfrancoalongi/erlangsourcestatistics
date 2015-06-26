-module(variable_stepping).
-compile({parse_transform,ess}).
-export([a/0]).

%% A -> A3
a() ->
    OldA = 1,
    A = b(OldA),
    A2 = m:f(NewA),
    A3 = m:ff(A2),
    c(A3).

%% NewA -> NewA3
b(OldA) ->
    NewA = m:f1(OldA),
    [ X || X <- lists:seq(1,100)],
    NewA1 = m:f2(NewA),
    B = 1,
    NewA2 = m:f3(NewA1,B),
    NewA3 = m:f4(NewA2),    
    NewA3.

%% X -> X4
%% Z -> Z4
c(OldA) ->
    case OldA of
        X when OldA > 10 ->
            X1 = m:f(X),
            X2 = m:f2(X1),
            X3 = m:f3(X2),
            X4 = m:f4(X3),
            X4;
        Y ->
            case Y of
                Z when Y < 100 ->
                    Z1 = m:f(Z),
                    Z2 = m:f2(Z1),
                    Z3 = m:f3(Z2),
                    Z4 = m:f4(Z3),
                    Z4
            end
    end.
