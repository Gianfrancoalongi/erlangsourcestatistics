-module(ess_ast).
-include("ess.hrl").

-export([traverse/4]).
-export([has_expression_children/1]).
-export([expression_children/1]).



traverse(AST = {function, _, Name, _, Clauses}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist), 
          [ traverse(X, NodeF, Gen, NewHist) || X <- Clauses ]);
traverse(L, NodeF, Gen, Hist) when is_list(L) ->
    NodeF(Hist#hist.base_element, 
          [ traverse(X, NodeF, Gen, Hist) || X <- L ]);
traverse(AST = {'case', _, Expr, Clauses}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist),
           traverse(Clauses, NodeF, Gen, NewHist)]);
traverse(AST = {match, _, LHS, RHS}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    HistLHS = set_hist(lhs, NewHist),
    HistRHS = set_hist(rhs, NewHist),
    NodeF(Gen(AST, NewHist),
          [traverse(LHS, NodeF, Gen, HistLHS),
           traverse(RHS, NodeF, Gen, HistRHS)]);
traverse(AST = {call, _, _, Args}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Args, NodeF, Gen, NewHist)]);

traverse(AST = {'try', _, CallExprs, Clauses, CatchClauses, _}, 
             NodeF, Gen, Hist)->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(CallExprs, NodeF, Gen, NewHist),
           traverse(Clauses, NodeF, Gen, NewHist),
           traverse(CatchClauses, NodeF, Gen, NewHist)]);

traverse(AST = {'catch', _, CallExpr}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(CallExpr, NodeF, Gen, NewHist)
          ]);

traverse(AST = {clause, _, Match, Guards, Exprs}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    MatchHist = set_hist(match, NewHist),
    NodeF(Gen(AST, NewHist),
          [traverse(Match, NodeF, Gen, MatchHist),
           traverse(Guards, NodeF, Gen, NewHist),
           traverse(Exprs, NodeF, Gen, NewHist)]);
 
traverse(AST = {bin, _, Elems}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Elems, NodeF, Gen, NewHist)]);

traverse(AST = {bin_element, _, Elems, _, _}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Elems, NodeF, Gen, NewHist)]);

traverse(AST = {'if', _, Clauses}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Clauses, NodeF, Gen, NewHist)]);

traverse(AST = {'receive', _, Clauses}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Clauses, NodeF, Gen, NewHist)]);

traverse(AST = {'receive', _, Clauses, _, AfterExprs}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Clauses, NodeF, Gen, NewHist),
           traverse(AfterExprs, NodeF, Gen, NewHist)]);

traverse(AST = {cons, _, Hd, Tl}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Hd, NodeF, Gen, NewHist),
           traverse(Tl, NodeF, Gen, NewHist)]);

traverse(AST = {record, _, _, Fields}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Fields, NodeF, Gen, NewHist)]);

traverse(AST = {record, _, Var, _, RecordField}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Var, NodeF, Gen, NewHist),
           traverse(RecordField, NodeF, Gen, NewHist)]);

traverse(AST = {record_field, _, _, Expr}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist)]);

traverse(AST = {record_field, _, Expr1, _, Expr2}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr1, NodeF, Gen, NewHist),
           traverse(Expr2, NodeF, Gen, NewHist)
          ]);

traverse(AST = {record_index, _, _, Expr}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist)]);

traverse(AST = {tuple, _, Elements}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Elements, NodeF, Gen, NewHist)]);

traverse(AST = {op, _, _, LHS, RHS}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(LHS, NodeF, Gen, NewHist),
           traverse(RHS, NodeF, Gen, NewHist)
          ]);

traverse(AST = {op, _, _, Expr}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist)
          ]);

traverse(AST = {lc, _, Body, Generator}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Body, NodeF, Gen, NewHist),
           traverse(Generator, NodeF, Gen, NewHist)
          ]);

traverse(AST = {generate, _, Expr, Guards}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist),
           traverse(Guards, NodeF, Gen, NewHist)
          ]);

traverse(AST = {b_generate, _, Expr, Guards}, NodeF, Gen, Hist) -> 
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist),
           traverse(Guards, NodeF, Gen, NewHist)
          ]);

traverse(AST = {'fun', _, Expr}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Expr, NodeF, Gen, NewHist)
          ]);

traverse(AST = {clauses, Clauses}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Clauses, NodeF, Gen, NewHist)
          ]);

traverse(AST = {block, _, CallExprs}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(CallExprs, NodeF, Gen, NewHist)
          ]);

traverse(AST = {bc, _, Body, Generator}, NodeF, Gen, Hist) ->
    NewHist = set_hist(AST, Hist),
    NodeF(Gen(AST, NewHist),
          [traverse(Body, NodeF, Gen, NewHist),
           traverse(Generator, NodeF, Gen, NewHist)
          ]);

traverse(AST = {function, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {function, _, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {nil, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {atom, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {var, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {string, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {integer, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {float, _, _}, _, Gen, Hist) -> Gen(AST, Hist);
traverse(AST = {char, _, _},  _, Gen, Hist) -> Gen(AST, Hist).


-define(HAS_EXPR_CHILDREN, ['clause', 'block', 'try']).

has_expression_children(AST) ->
    lists:member(ast_type(AST), ?HAS_EXPR_CHILDREN).

ast_type(AST) ->
    element(1, AST).

expression_children({clause, _, _, _, Exprs}) ->
    Exprs;
expression_children({'try', _, CallExprs, Clauses, CatchClauses, _}) ->
    CallExprs ++ Clauses ++ CatchClauses;
expression_children({'fun', _, Expr}) ->
    Expr;
expression_children({block, _, CallExprs}) ->
    CallExprs;
expression_children(_) ->
    [].

    
set_hist(AST, H) when is_tuple(AST) ->
    Type = ast_type(AST),
    set_hist(Type, H);
set_hist('case', H=#hist{'case'=C}) -> H#hist{'case'=C+1};
set_hist('match', H=#hist{'match'=C}) -> H#hist{'match'=C+1};
set_hist('lhs', H=#hist{lhs=C}) -> H#hist{lhs=C+1};
set_hist('rhs', H=#hist{rhs=C}) -> H#hist{rhs=C+1};
set_hist('call', H=#hist{'call'=C}) -> H#hist{'call'=C+1};
set_hist('try', H=#hist{'try'=C}) -> H#hist{'try'=C+1};
set_hist('catch', H=#hist{'catch'=C}) -> H#hist{'catch'=C+1};
set_hist('function', H=#hist{'function'=C}) -> H#hist{'function'=C+1};
set_hist('clause', H=#hist{'clause'=C}) -> H#hist{'clause'=C+1};
set_hist('clauses', H=#hist{'clauses'=C}) -> H#hist{'clauses'=C+1};
set_hist('bin', H=#hist{'bin'=C}) -> H#hist{'bin'=C+1};
set_hist('bin_element', H=#hist{'bin_element'=C}) -> H#hist{'bin_element'=C+1};
set_hist('if', H=#hist{'if'=C}) -> H#hist{'if'=C+1};
set_hist('receive', H=#hist{'receive'=C}) -> H#hist{'receive'=C+1};
set_hist('cons', H=#hist{'cons'=C}) -> H#hist{'cons'=C+1};
set_hist('record', H=#hist{'record'=C}) -> H#hist{'record'=C+1};
set_hist('record_field', H=#hist{'record_field'=C}) -> H#hist{'record_field'=C+1};
set_hist('record_index', H=#hist{'record_index'=C}) -> H#hist{'record_index'=C+1};
set_hist('tuple', H=#hist{'tuple'=C}) -> H#hist{'tuple'=C+1};
set_hist('op', H=#hist{'op'=C}) -> H#hist{'op'=C+1};
set_hist('lc', H=#hist{'lc'=C}) -> H#hist{'lc'=C+1};
set_hist('generate', H=#hist{'generate'=C}) -> H#hist{'generate'=C+1};
set_hist('b_generate', H=#hist{'b_generate'=C}) -> H#hist{'b_generate'=C+1};
set_hist('fun', H=#hist{'fun'=C}) -> H#hist{'fun'=C+1};
set_hist('block', H=#hist{'block'=C}) -> H#hist{'block'=C+1};
set_hist('bc', H=#hist{'bc'=C}) -> H#hist{'bc'=C+1}.
