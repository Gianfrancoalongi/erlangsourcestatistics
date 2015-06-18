-module(ess).
-export([parse_transform/2]).

parse_transform(AST,Options) ->    
    R = number_of_expressions_per_line(AST),
    R2= number_of_expressions_per_function(AST),
    R3= number_of_functions_per_module(AST),
    R4= number_of_function_clauses_per_function(AST),
    report([{number_of_expressions_per_line,R},
            {number_of_expressions_per_function,R2},
            {number_of_functions_per_module,R3},
            {number_of_function_clauses_per_function,R4}
           ],Options),
    AST.

number_of_expressions_per_line(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    LNs = lists:flatten([ get_linenumbers(F) || F <- Fs ]),
    ROSL = repeats_on_same_line(LNs,undefined,0,[]),
    hide_anything_under_2(ROSL).

number_of_expressions_per_function(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    [ {element(3,F), element(4,F), 
       begin
           LNs = get_linenumbers(F),
           length(lists:usort(lists:flatten(LNs)))
       end}
      || F <- Fs ].

number_of_functions_per_module(AST) ->
    length([ X || X <- AST, element(1,X) == function]).
    
number_of_function_clauses_per_function(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    [ {element(3,F), element(4,F), length(element(5,F))}
      || F <- Fs].

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
