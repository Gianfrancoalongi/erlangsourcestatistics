-module(bad_clauses).
-export([f/0]).

f() ->
    P = [1,a,2,{ok,1},b,3,{error,2},c, -2, -10, 20],
    {Ints, Atoms,Tuples} = collect(P,[],[],[]),
    {Ints,Atoms,Tuples}.

collect([I|R],Ints,Atoms,Tuples) when is_integer(I) ->
    collect(R,[I|Ints],Atoms,Tuples);
collect([A|R],Ints,Atoms,Tuples) when is_atom(A) ->
    collect(R,Ints,[A|Atoms],Tuples);
collect([T|R],Ints,Atoms,Tuples) when is_tuple(T) ->
    collect(R,Ints,Atoms,[T|Tuples]);
collect([T|R],Ints,Atoms,Tuples) when not is_tuple(T) ->
    collect(R,Ints,Atoms,[T|Tuples]);
collect([],Ints,Atoms,Tuples) ->
    {Ints, Atoms, Tuples}.

