-module(ess).
-export([parse_transform/2]).

parse_transform(AST,Options) ->    
    R = number_of_expressions_per_line(AST),
    R2= number_of_expressions_per_function(AST),
    R3= number_of_functions_per_module(AST),
    R4= number_of_function_clauses_per_function(AST),
    R5= number_of_record_definitions_per_module(AST),
    R6= number_of_includes_per_module(AST),
    R7= variable_steppings(AST),
    R8= structural_depth(AST),
    report([{number_of_expressions_per_line,R},
            {number_of_expressions_per_function,R2},
            {number_of_functions_per_module,R3},
            {number_of_function_clauses_per_function,R4},
            {number_of_record_definitions_per_module,R5},
            {number_of_includes_per_module,R6},
            {variable_steppings, R7},
            {structural_depth, R8}
           ],Options),
    AST.

number_of_expressions_per_line(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    LNs = lists:flatten([ get_linenumbers(F) || F <- Fs ]),
    ROSL = repeats_on_same_line(LNs,undefined,0,[]),
    hide_anything_under_2(ROSL).

number_of_expressions_per_function(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    [ {function_identity(F),
       begin
           LNs = get_linenumbers(F),
           length(lists:usort(lists:flatten(LNs)))
       end}
      || F <- Fs ].

function_identity(F) ->
    {element(3,F),element(4,F)}.
function_clauses(F) ->
    element(5, F).

number_of_functions_per_module(AST) ->
    io:format("AST:~p~n",[AST]),
    length([ X || X <- AST, element(1,X) == function]).
    
number_of_function_clauses_per_function(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    [ {function_identity(F), length(element(5,F))}
      || F <- Fs].

number_of_record_definitions_per_module(AST) ->
    length([ 1 || {attribute,_,record,_} <- AST ]).

number_of_includes_per_module(AST) ->
    Files = [ F || {attribute,_,file,_}=F <- AST],
    [{attribute,_,file,{FileName,_}}|R] = Files,
    Others = [ F || F <- R, element(1,element(4,F)) =/= FileName ],                     
    length(Others).

variable_steppings(AST) ->
    ClausesSet = [ Clauses || {function,_,_,_,Clauses} <- AST ],
    [ variable_steppings_in_body(ClausesList) || ClausesList <- ClausesSet ].

variable_steppings_in_body([]) ->
    [];
variable_steppings_in_body([{clause,_,Arguments,_,Body}|T]) ->
    Arg_Variables = extract_variables(Arguments),
    Body_Variables = extract_variables(Body),
    R = steppings(Arg_Variables++Body_Variables),
    R ++ variable_steppings_in_body(T).

extract_variables([]) ->
    [];
extract_variables([{var,_,V}=V|R]) ->
    [atom_to_list(V) | extract_variables(R)];
extract_variables([E|R]) when is_tuple(E) ->
    extract_variables(tuple_to_list(E))++
        extract_variables(R);
extract_variables([E|R]) when is_list(E) ->
    extract_variables(E)++
        extract_variables(R);
extract_variables([_|R]) ->
    extract_variables(R).


steppings(Variables) ->
    Variables.

structural_depth(AST) ->
    Functions = [F || F <- AST, is_ast_function(F) ],
    [ {function_identity(F), gen_structural_depth(function_clauses(F))} || 
	F <- Functions ].

gen_structural_depth(L) when is_list(L) ->
    lists:sum([ gen_structural_depth(X) || X <- L ]);
gen_structural_depth({clause, _, Match, Guards, Exprs}) ->
    2*gen_structural_depth(Match) 
	+ gen_structural_depth(Guards) 
	+ gen_structural_depth(Exprs);
gen_structural_depth({match,_,RHS,LHS}) ->
    2*gen_structural_depth(RHS) + gen_structural_depth(LHS);
gen_structural_depth({call,_, _, Args}) ->
    1+gen_structural_depth(Args);
gen_structural_depth({'case',_, Expr, Clauses}) ->
    gen_structural_depth(Expr) + gen_structural_depth(Clauses);
gen_structural_depth({cons,_,Hd, Tl}) ->
    gen_structural_depth(Hd) + gen_structural_depth(Tl);
gen_structural_depth({tuple,_,Elements}) ->
    1+gen_structural_depth(Elements);
gen_structural_depth({op,_,_,LHS,RHS}) ->
    gen_structural_depth(LHS) + gen_structural_depth(RHS);
gen_structural_depth({op,_,_,Expr}) ->
    gen_structural_depth(Expr);

gen_structural_depth({atom,_,_}) -> 0;
gen_structural_depth({var,_,_}) -> 0;
gen_structural_depth({integer,_,_}) -> 0;

gen_structural_depth(What) ->
    io:format("**** unknown AST: ~p~n", [What]),
    0.


is_ast_function(F) when element(1, F) == function ->
    true;
is_ast_function(_) ->
    false.


hide_anything_under_2(Repeats_per_line) ->
    [ X || X <- Repeats_per_line, element(2,X) > 1 ].

repeats_on_same_line([],_,1,Acc) ->
    lists:reverse(Acc);
repeats_on_same_line([],N,Seen,Acc) ->
    lists:reverse([{N,Seen}|Acc]);
repeats_on_same_line([N|R],undefined,0,[]) ->
    repeats_on_same_line(R,N,1,[]);
repeats_on_same_line([N|R],N,Seen,Acc) ->
    repeats_on_same_line(R,N,Seen+1,Acc);
repeats_on_same_line([N|R],L,Seen,Acc) ->
    repeats_on_same_line(R,N,1,[{L,Seen}|Acc]).

get_linenumbers({function,_Line,_,_,Clauses}) ->    
    [ get_linenumbers(C) || C <- Clauses ];
get_linenumbers({clause,_Line,_,_,Expressions}) ->
    get_linenumbers_body(Expressions).

get_linenumbers_body([]) ->
    [];
get_linenumbers_body([{'case',L,_,Clauses}|R]) ->
    CaseLines = get_linenumbers_body_clause(Clauses),
    [L|CaseLines]++get_linenumbers_body(R);
get_linenumbers_body([{_,L,_,_}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{_,L,_}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{nil,L}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{op,L,_,_,_}|R]) ->
    [L|get_linenumbers_body(R)].


get_linenumbers_body_clause([]) ->
    [];
get_linenumbers_body_clause([{clause,L,_,_,Expressions}|R]) ->
    BodyLines = get_linenumbers_body(Expressions),
    [L|BodyLines] ++ get_linenumbers_body_clause(R).

report(Results,Options) ->
    io:format("Options:~p~n",[Options]),
    [ io:format("~p ~p~n",[Tag,Value]) || {Tag,Value} <- Results].
