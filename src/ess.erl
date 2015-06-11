-module(ess).
-export([parse_transform/2]).

parse_transform(AST,Options) ->    
    R = number_of_expressions_per_line(AST),
    report([{number_of_expressions_per_line,R}],Options),
    AST.

number_of_expressions_per_line(AST) ->
    Fs = [ X || X <- AST, element(1,X) == function],
    LNs = lists:flatten([ get_linenumbers(F) || F <- Fs ]),
    ROSL = repeats_on_same_line(LNs,undefined,0,[]),
    hide_anything_under_2(ROSL).

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
    repeats_on_same_line(R,N,1,[{N,Seen}|Acc]).

get_linenumbers({function,_Line,call,_,Clauses}) ->    
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
    [L|get_linenumbers_body(R)].


get_linenumbers_body_clause([]) ->
    [];
get_linenumbers_body_clause([{clause,L,_,_,Expressions}|R]) ->
    BodyLines = get_linenumbers_body(Expressions),
    [L|BodyLines] ++ get_linenumbers_body_clause(R).

report([{Tag,Value}],Options) ->
    io:format("Options:~p~n",[Options]),
    io:format("~p ~p~n",[Tag,Value]).
