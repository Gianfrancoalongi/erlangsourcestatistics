-module(ess).
-compile(export_all).

file(F) ->
    file(F, []).
file(F, Opts) ->
    {ok,Mod,Bin} = compile:file(F,[binary,debug_info]),
    {ok,{Mod,[{abstract_code,{raw_abstract_v1,AST}}]}} = 
        beam_lib:chunks(Bin,[abstract_code]),
    analyse(AST,Opts).

analyse(AST, _Opts) -> 
    io:format("Analysing:~p~n",[AST]).

analyze_function(AST) ->
    sort([{depth, structural_depth(AST)},
          {expressions_per_function, number_of_expressions_for_function(AST)},
          {clauses, clauses_per_function(AST)},
          {arity, function_arity(AST)},
          {expressions_per_line, expressions_per_function_line(AST)},
          {variable_steppings, variable_steppings_per_function(AST)}
         ]).

sort(L) -> lists:sort(L).

usort(L) -> lists:usort(L).
    
expressions_per_function_line(AST) ->
    LNs = get_linenumbers(AST),
    ROSL = repeats_on_same_line(lists:flatten(LNs)),
    {lists:max(ROSL), lists:min(ROSL), avg_sum(ROSL)}.

avg_sum(L) ->
    round(lists:sum(L) / length(L)).

number_of_expressions_for_function(AST) ->
    LNs = get_linenumbers(AST),
    length(lists:usort(lists:flatten(LNs))).

function_clauses(F) ->
    element(5, F).

function_arity(AST) ->
    element(4, AST).

clauses_per_function(AST) ->
    length(function_clauses(AST)).

variable_steppings_per_function({function,_,_,_,Clauses}) ->
    sum([ variable_steppings_in_body(Clause) || Clause <- Clauses ]).

variable_steppings_in_body({clause,_,Arguments,_,Body}) ->
    Arg_Variables = extract_variables(Arguments),
    Body_Variables = extract_variables(Body),
    Variables = usort(Arg_Variables++Body_Variables),
    stepping(Variables).

sum(X) -> lists:sum(X).

extract_variables([{var,_,V}|R]) ->
    [atom_to_list(V) | extract_variables(R)];
extract_variables([E|R]) when is_tuple(E) ->
    extract_variables(tuple_to_list(E))++
        extract_variables(R);
extract_variables([E|R]) when is_list(E) ->
    extract_variables(E)++
        extract_variables(R);
extract_variables([_|R]) ->
    extract_variables(R);
extract_variables(_) ->
    [].

stepping(Vars) ->
    trailing_int(Vars) + leading_new(Vars) + leading_old(Vars).

trailing_int([]) ->
    0;
trailing_int([_]) ->
    0;
trailing_int([V1, V2 | R]) ->
    case is_variable_stepping(V1, V2) of
        true ->
            1 + trailing_int([V2 | R]);
        _ ->
            trailing_int([V2 | R])
    end.

leading_new(Vars) ->
    leading_string("New", Vars).

leading_old(Vars) ->
    leading_string("Old", Vars).


leading_string(Str, Vars) ->
    News = [ V || V <- Vars, is_leading_str(Str, V)],
    Others = Vars -- News,
    Found = lists:filter(
        fun(Other) ->
            lists:member(Str++Other, News)
        end,
        Others),
    length(Found).

is_leading_str(Str, Name) ->
    case string:str(Name, Str) of
        1 -> true;
        _ -> false
    end.

is_variable_stepping([X|V1], [X|V2]) ->
    is_variable_stepping(V1, V2);
is_variable_stepping([], V2) ->
    is_all_integers(V2);
is_variable_stepping(V1, V2) ->
    is_all_integers(V1) andalso is_all_integers(V2).

is_all_integers(L) ->
    io:format("is all int: ~p~n", [L]),
    lists:all(fun is_ascii_integer/1, L).

is_ascii_integer(X) when (X>=$0), (X=<$9) -> true; 
is_ascii_integer(_) -> false.


structural_depth(L) when is_list(L) ->
    lists:sum([ structural_depth(X) || X <- L ]);
structural_depth({function, _, _, _, Clauses}) ->
    structural_depth(Clauses);
structural_depth({clause, _, Match, Guards, Exprs}) ->
    structural_depth(Match) 
	+ structural_depth(Guards) 
	+ structural_depth(Exprs);
structural_depth({match,_,RHS,LHS}) ->
    1+structural_depth(RHS) + structural_depth(LHS);
structural_depth({call,_, _, Args}) ->
    1+structural_depth(Args);
structural_depth({bin,_, Elems}) ->
    1+structural_depth(Elems);
structural_depth({bin_element,_, Elem, _, _}) ->
    structural_depth(Elem);
structural_depth({'case',_, Expr, Clauses}) ->
    1 + structural_depth(Expr) + structural_depth(Clauses);
structural_depth({'if',_, Clauses}) ->
    1 + structural_depth(Clauses);
structural_depth({'receive',_,Clauses}) ->
    1 + structural_depth(Clauses);
structural_depth({'receive',_,Clauses, _, AfterExprs}) ->
    1 + structural_depth(Clauses) + structural_depth(AfterExprs);
structural_depth({cons,_,Hd, Tl}) ->
    structural_depth(Hd) + structural_depth(Tl);
structural_depth({record,_,_,Fields}) ->
    1+structural_depth(Fields);
structural_depth({record_field,_,_,Expr}) ->
    1+structural_depth(Expr);
structural_depth({tuple,_,Elements}) ->
    1+structural_depth(Elements);
structural_depth({op,_,_,LHS,RHS}) ->
    1+structural_depth(LHS) + structural_depth(RHS);
structural_depth({op,_,_,Expr}) ->
    1+structural_depth(Expr);
structural_depth({lc,_,Body,Generator}) ->
    1+structural_depth(Body)+structural_depth(Generator);
structural_depth({generate,_,Expr,Guards}) ->
    structural_depth(Expr) + structural_depth(Guards);

structural_depth({nil,_}) -> 0;
structural_depth({atom,_,_}) -> 0;
structural_depth({var,_,_}) -> 0;
structural_depth({string,_,_}) -> 0;
structural_depth({integer,_,_}) -> 0.


repeats_on_same_line(LNs) ->
    repeats_on_same_line(LNs,hd(LNs),0).

repeats_on_same_line([N|R],N,Counted) ->
    repeats_on_same_line(R,N,Counted+1);
repeats_on_same_line([N|R],_,Counted) ->
    [ Counted | repeats_on_same_line(R,N,1) ];
repeats_on_same_line([],_,Counted) ->
    [ Counted ].


get_linenumbers({function,_Line,_,_,Clauses}) ->    
    [ get_linenumbers(C) || C <- Clauses ];
get_linenumbers({clause,_Line,_,_,Expressions}) ->
    get_linenumbers_body(Expressions).

get_linenumbers_body([]) ->
    [];
get_linenumbers_body([{Marker,LN,_}|T]) when is_atom(Marker) ->
    [LN|get_linenumbers_body(T)];
get_linenumbers_body([{'case',L,_,Clauses}|R]) ->
    CaseLines = get_linenumbers_body(Clauses),
    [L|CaseLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'receive',L,_,Clauses}|R]) ->
    CaseLines = get_linenumbers_body(Clauses),
    [L|CaseLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'call',L,_, Args}|R]) ->
    ArgsLines = get_linenumbers_body(Args),
    [L|ArgsLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{clause,L,_,_,Expressions}|R]) ->
    BodyLines = get_linenumbers_body(Expressions),
    [L|BodyLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{nil,L}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{op,L,_,_,_}|R]) ->
    [L|get_linenumbers_body(R)].


