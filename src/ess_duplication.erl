-module(ess_duplication).
-include("ess.hrl").

-export([detect/2]).
-export([check/1]).
-export([assure_tab/0]).

-import(ess_lib, [gv/2, gv/3]).

-define(MINIMUM_STRUCTURAL_DUPLICATION_DEPTH, 16).


check(Tree) ->
    ess_lib:reset_log(dup),
    Duplicates = ets_walk(),
    report_duplicates_to_log_file(Duplicates),
    T2 = insert_duplicate_info_in_tree(Duplicates, Tree),
    generate_toplevel_duplication_count(T2, Duplicates).

generate_toplevel_duplication_count(T=#tree{raw_values=RV}, Duplicates) ->
    Count = lists:sum([ length(L)-1 || {_, L} <- Duplicates ]),
    T#tree{raw_values=[{total_duplicates, Count} | RV]}.

insert_duplicate_info_in_tree(Duplicates, Tree) ->
    ModCounts = count_duplication_per_module(Duplicates),
    decorate_tree(ModCounts, Tree).

decorate_tree(ModCounts, T = #tree{type=dir, children=CS}) ->
    CS2 = [ decorate_tree(ModCounts, C) || C <- CS ],
    T#tree{children = CS2};
decorate_tree(ModCounts, T = #tree{type=file, name=Name, raw_values=RV}) ->
    Mod = file_name_to_mod(Name),
    Count = gv(Mod, ModCounts, 0),
    Elem = {code_duplicates, Count},
    T#tree{raw_values= [Elem | RV]}.

file_name_to_mod(N) ->
    list_to_atom(filename:basename(N, ".erl")).

count_duplication_per_module(Duplicates) ->
    AllDups = lists:sort(lists:flatten([ X || {_, X} <- Duplicates ])),
    Mods = [ K || {K,_} <- AllDups],
    count_mods(Mods).

count_mods([Mod | L]) -> 
    count_mods(Mod, 1, L).

count_mods(Mod, Count, []) ->
    [{Mod, Count}];
count_mods(Mod, Count, [Mod | L]) ->
    count_mods(Mod, Count+1, L);
count_mods(Mod, Count, [NewMod | L]) ->
    [{Mod, Count} | count_mods(NewMod, 1, L)].

report_duplicates_to_log_file(L) ->
    [ report_duplicate(X) || X <- L ].

report_duplicate({Code, Info}) ->
    ess_lib:log(dup, 
                "---------~n"
                "~p~n"
                "~p~n~n", [Info, Code]).

detect(Module, ModuleAST) ->
    AST = pick_functions(ModuleAST),
    put(module, Module),
    trav(AST).

trav({attribute, _, _, _}) ->
    attribute;

trav({function, Line, _, _, Clauses}) ->
    Anon = {function, name, trav(Clauses)},
    maybe_store(Anon, Line);

trav(L) when is_list(L) ->
    [ trav(X) || X <- L ];

trav({'case', Line, Expr, Clauses}) ->
    Anon = {'case', trav(Expr), trav(Clauses)},
    maybe_store(Anon, Line);

trav({match, Line, LHS, RHS}) ->
    Anon = {match, trav(LHS), trav(RHS)},
    maybe_store(Anon, Line);

trav({call, Line, _, Args}) ->
    Anon = {call, trav(Args)},
    maybe_store(Anon, Line);

trav({'try', Line, CallExprs, Clauses, CatchClauses, After}) ->
    Anon = {'try', trav(CallExprs), trav(Clauses), trav(CatchClauses), trav(After)},
    maybe_store(Anon, Line);

trav({'catch', Line, CallExpr}) ->
    Anon = {'catch', trav(CallExpr)},
    maybe_store(Anon, Line);

trav({clause, Line, Match, Guards, Exprs}) ->
    Anon = {clause, trav(Match), trav(Guards), trav(Exprs)},
    maybe_store(Anon, Line);
 
trav({bin, Line, Elems}) ->
    Anon = {bin, trav(Elems)},
    maybe_store(Anon, Line);

trav({bin_element, Line, Elems, _, _}) ->
    Anon = {bin_element, trav(Elems)},
    maybe_store(Anon, Line);

trav({'if', Line, Clauses}) ->
    Anon = {'if', trav(Clauses)},
    maybe_store(Anon, Line);

trav({'receive', Line, Clauses}) ->
    Anon = {'receive', trav(Clauses)},
    maybe_store(Anon, Line);

trav({'receive', Line, Clauses, _, AfterExprs}) ->
    Anon = {'receive', trav(Clauses), trav(AfterExprs)},
    maybe_store(Anon, Line);

trav({cons, Line, Hd, Tl}) ->
    Anon = {cons, trav(Hd), trav(Tl)},
    maybe_store(Anon, Line);

trav({record, Line, _, Fields}) ->
    Anon = {record, trav(Fields)},
    maybe_store(Anon, Line);

trav({record, Line, Var, _, RecordField}) ->
    Anon = {record, trav(Var), trav(RecordField)},
    maybe_store(Anon, Line);

trav({record_field, Line, _, Expr}) ->
    Anon = {record_field, trav(Expr)},
    maybe_store(Anon, Line);

trav({record_field, Line, Expr1, _, Expr2}) ->
    Anon = {record_field, trav(Expr1), trav(Expr2)},
    maybe_store(Anon, Line);

trav({record_index, Line, _, Expr}) ->
    Anon = {record_index, trav(Expr)},
    maybe_store(Anon, Line);

trav({tuple, Line, Elements}) ->
    Anon = {tuple, trav(Elements)},
    maybe_store(Anon, Line);

trav({op, Line, _, LHS, RHS}) ->
    Anon = {op, trav(LHS), trav(RHS)},
    maybe_store(Anon, Line);

trav({op, Line, _, Expr}) ->
    Anon = {op, trav(Expr)},
    maybe_store(Anon, Line);

trav({lc, Line, Body, Generator}) ->
    Anon = {lc, trav(Body), trav(Generator)},
    maybe_store(Anon, Line);

trav({generate, Line, Expr, Guards}) ->
    Anon = {generate, trav(Expr), trav(Guards)},
    maybe_store(Anon, Line);

trav({b_generate, Line, Expr, Guards}) ->
    Anon = {b_generate, trav(Expr), trav(Guards)},
    maybe_store(Anon, Line);

trav({'fun', Line, Expr}) ->
    Anon = {'fun', trav(Expr)},
    maybe_store(Anon, Line);

trav({clauses, Clauses}) ->
    {clauses, trav(Clauses)};

trav({block, Line, CallExprs}) ->
    Anon = {block, trav(CallExprs)},
    maybe_store(Anon, Line);

trav({bc, Line, Body, Generator}) ->
    Anon = {bc, trav(Body), trav(Generator)},
    maybe_store(Anon, Line);

trav({function, _, _}) -> {function, name};
trav({function, _, _, _}) -> {function, name};
trav({nil, _}) -> nil;
trav({atom, _, _}) -> atom;
trav({var, _, _}) -> var;
trav({string, _, _}) -> string;
trav({integer, _, _}) -> integer;
trav({float, _, _}) -> float;
trav({char, _, _}) -> char;
trav({eof, _}) -> eof.

maybe_store(Anon, Line) ->
    case is_big_enough(Anon) of
        true ->
            Info = {get(module), Line},
            store(Anon, Info);
        _ ->
            ok
    end,
    Anon.

is_big_enough(X) when element(1, X) == cons ->
    false;
is_big_enough(X) ->
    depth(X, 1) > ?MINIMUM_STRUCTURAL_DUPLICATION_DEPTH.

depth(L, D) when is_tuple(L) ->
    depth(tuple_to_list(L), D);
depth(L, D) when is_list(L) ->
    dmax([ depth(E, D+1) || E <- L ]);
depth(_, D) ->
    D.
    
dmax([]) ->
    0;
dmax(L) ->
    lists:max(L).

pick_functions(ModuleAST) ->
    [ A || A <- ModuleAST, is_ast_function(A) ].

is_ast_function(X) when element(1,X) == function -> true;
is_ast_function(_) -> false.

    
assure_tab() ->
    case tab_exist(?MODULE) of
        true -> ok;
        _ -> init_tab(?MODULE)
    end.

tab_exist(Tab) ->
    undefined =/= ets:info(Tab, size).

init_tab(Tab) ->
    ets:new(Tab, [bag, public, named_table]).

store(Key, Val) ->
    ets:insert(?MODULE, {Key, Val}).

ets_walk() ->
    ets_walk(ets:first(?MODULE)).

ets_walk('$end_of_table') ->
    [];
ets_walk(Key) ->
    NextKey = ets:next(?MODULE, Key),
    case ets:lookup(?MODULE, Key) of
        L when length(L) > 1 -> 
            [ {Key, value_from_entries(L)} | ets_walk(NextKey)];
        _ -> 
            ets_walk(NextKey)
    end.

value_from_entries(L) ->
    [ V || {_, V} <- L ].

    %% ets:foldl(
    %%   fun(Entry, Acc) ->
    %%           case is_duplicate(Entry) of
    %%               true -> [Entry | Acc];
    %%               _ -> Acc
    %%           end
    %%   end,
    %%   [],
    %%   ?MODULE).

%% is_duplicate({_, L}) -> 
%%     length(L) > 1.
