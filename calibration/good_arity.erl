-module(good_arity).
-export([f/0]).
-record(params,{a,b,c,d,e,f}).

f() ->
    P = #params{a = 1,
                b = [1,2,3],
                c = {ok,2},
                d = undefined,
                e = undefined,
                f = 2},
    g(P).


g(P=#params{c=C, f=F}) when F > 3 ->
    case C of
        {ok, F} ->
            g(P);
        _ ->
            g(dec_f(P))
    end;
g(P=#params{b = B}) when length(B) < 4 ->
    case B of
        [1,2,3] ->
            g(P);
        [] ->
            g(P)
    end;
g(P) ->
    NewB = l(P),
    g(update_b(P,NewB)).
        
dec_f(P=#params{f=F}) ->
    P#params{f=F-1}.

update_b(P, B) -> 
    P#params{b = B}.

l(#params{b = B}) ->
    tl(B).
