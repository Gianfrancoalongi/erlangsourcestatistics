-module(ess_duplication).
-include("ess.hrl").

-export([detect/2]).
-export([check/1]).
-export([assure_tab/0]).

check(AST) ->
    ets_walk(ets:first(?MODULE)),
    AST.

ets_walk('$end_of_table') ->
    ok;
ets_walk(Key) ->
    case ets:lookup(?MODULE, Key) of
        L when length(L) > 1 -> 
            report_duplicate(L);
        _ ->
            ok
    end,
    ets_walk(ets:next(?MODULE, Key)).

report_duplicate(L) ->
    Code = element(1, hd(L)),
    Lines = [ element(2, X) || X <- L ],
    io:format("DUPLICATE: ~p ~p~n",
              [Code, Lines]).
    

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
    depth(X, 1) > 7.

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


