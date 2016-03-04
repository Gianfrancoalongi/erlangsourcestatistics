-module(ess).
-include("ess.hrl").
-compile(export_all).

get_compile_include_path([]) ->
    [];
get_compile_include_path(IncFilePath) ->
    case file:read_file(IncFilePath) of
        {ok, Data} ->
	    Lines = binary:split(Data, [<<"\n">>], [global]),
	    [{i, binary_to_list(Line)} || Line <- Lines, Line =/= <<>>];
	_ ->
	    []
    end.

dir(F) ->
    dir(F, [], []).
dir(F, Opts) ->
    dir(F, Opts, []).
dir(F, Opts, IncFile) ->
    Tree = recursive_dir([F]),
    ForEachFileFun = fun(File) -> file(File, Opts, IncFile) end,
    hd(traverse(Tree, ForEachFileFun)).

traverse([{Dir,Files,SubDirs}|R], Fun) ->
    Stats = for_each_file(Files, Fun) ++ traverse(SubDirs, Fun),
    Aggregated = aggregate_trees(Stats),
    Res = #tree{type = dir,
                name = Dir,
                value = sort(Aggregated),
                children = Stats},
    [ Res | traverse(R, Fun) ];

traverse([], _Fun) ->
    [].

for_each_file(Files, Fun) ->
    [ Fun(File) || File <- Files ].

recursive_dir([]) -> 
    [];
recursive_dir([Dir|R]) ->
    {ok, Files} = file:list_dir(Dir),    
    FullNames = [ filename:join(Dir,F) || F <- Files ],
    DirFiles = [ F || F <- FullNames, not filelib:is_dir(F), 
                      is_erlang_source_file(F) ],
    Dirs = [ F || F <- FullNames, filelib:is_dir(F) ],
    RecursiveStuff = recursive_dir(Dirs),
    Res = case {DirFiles, RecursiveStuff} of
              {[], []} ->
                  [];
              _ -> 
                  [{Dir, DirFiles, recursive_dir(Dirs)}]
          end,            
    Res  ++ recursive_dir(R).

is_erlang_source_file(F) ->
    filename:extension(F) == ".erl".

get_all_files(Folder) ->
    filelib:fold_files(Folder, ".*.erl$", true, 
                       fun(File, AccIn) -> [File | AccIn] end, 
                       []).

file(F) ->
    file(F, [], []).
file(F, Opts) ->
    file(F, Opts, []).
file(F, Opts, IncFile) ->
    IncPath = get_compile_include_path(IncFile),
    CompileOpts = get_compile_options(),
    {ok,Mod,Bin,Warnings} = compile:file(F,CompileOpts ++ IncPath),
    {ok,{Mod,[{abstract_code,{raw_abstract_v1,AST}}]}} = 
        beam_lib:chunks(Bin,[abstract_code]),    
    Value = [warning_metric(Warnings)|analyse(AST, Opts)]++lexical_analyse(F, Opts),
    #tree{type = file,
          name = F,
          value = sort(Value)
         }.

get_compile_options() ->
    [binary,verbose, debug_info, return].

lexical_analyse(F, _Opts) ->
    {ok, Bin} = file:read_file(F),
    lexical_analyse_string(binary_to_list(Bin)).

lexical_analyse_string(Str) ->
    handle_comment_percent(count_comment_and_code_lines(strip_lines(divide_into_lines(Str)))).

count_comment_and_code_lines(L) ->
    Tot = length(L),
    LineLengths = line_lengths(L),
    {Code, Comment, Blank} = count_comment_and_code_lines2(L, 0, 0, 0),
    [{total_lines, Tot},
     {lines_of_code, Code}, 
     {lines_of_comments, Comment}, 
     {line_lengths, LineLengths},
     {blank_lines, Blank}].

count_comment_and_code_lines2([], Code, Comment, Blank) ->
    {Code, Comment, Blank};
count_comment_and_code_lines2([[] | Ls], Code, Comment, Blank) ->
    count_comment_and_code_lines2(Ls, Code, Comment, Blank+1);
count_comment_and_code_lines2([L | Ls], Code, Comment, Blank) ->
    case is_comment_line(L) of
	true ->
	    count_comment_and_code_lines2(Ls, Code, Comment+1, Blank);
	_ ->
	    count_comment_and_code_lines2(Ls, Code+1, Comment, Blank)
    end.

is_comment_line("%"++_) -> true;
is_comment_line(_) -> false.

line_lengths(Ls) ->
    N = length(Ls),
    Lengths =[ length(L) || L <- Ls ],
    Max = lists:max(Lengths),
    Min = lists:min(Lengths),
    Sum = lists:sum(Lengths),
    Mean = round(Sum / N),
    #val{max=Max, min=Min, avg=Mean, sum=Sum, n=N}.

divide_into_lines(Str) ->
    dil(Str,[],[]).

dil([],[],Res) ->
    rev(Res);
dil([],Current,Res) ->
    rev([rev(Current)|Res]);
dil([$\n|R],Current,Acc) ->
    dil(R,[],[rev(Current)|Acc]);
dil([C|R],Current,Acc) ->
    dil(R,[C|Current],Acc).


strip_lines(Ls) ->
    [rev(remove_ws(rev(remove_ws(L)))) || L <- Ls ].

remove_ws(L) ->
    lists:dropwhile(fun(C) -> lists:member(C, [32,9]) end, L).

analyse(AST, _Opts) -> 
    Fs = [ analyze_function(F) || F <- AST, is_ast_function(F) ],
    aggregate(Fs).

aggregate_trees(Trees) ->
    handle_comment_percent(aggregate(extract_values(Trees))).

handle_comment_percent(L) ->
    Percent = calculate_comment_to_line_percent(L),
    replace_tag(comment_to_line_percent, Percent, L).

calculate_comment_to_line_percent(L) ->
    Lines = value_sum(gv(total_lines,L)),
    Comments = value_sum(gv(lines_of_comments,L)),
    round(100*(Comments/Lines)).

extract_values(Trees) ->
    [ T#tree.value || T <- Trees ].

aggregate(Values) ->
    group_on_tag(Values).

group_on_tag(Fs) ->
    Keys = proplists:get_keys(hd(Fs)),
    All_results = lists:flatten(Fs),
    aggregate2([ {Key, proplists:get_all_values(Key,All_results)} || 
                   Key <- Keys ]).

aggregate2(L) ->
    [ {Key,aggregate_values(Values)} || {Key,Values} <- L].

aggregate_values(L) ->
    Max = lists:max(get_max_values(L)),
    Min = lists:min(get_min_values(L)),
    Sum = sum(get_sum_values(L)),
    N = count_number_of_items(L),
    Mean = round(Sum / N),
    #val{max=Max, min=Min, avg=Mean, sum=Sum, n=N}.

get_max_values(L) -> [value_max(X) || X <- L].
get_min_values(L) -> [value_min(X) || X <- L].
get_sum_values(L) -> [value_sum(X) || X <- L].

value_max(#val{max=M}) -> M;
value_max(M) when is_integer(M) -> M.

value_min(#val{min=M}) -> M;
value_min(M) when is_integer(M) -> M.

value_sum(#val{sum=Sum}) -> Sum;
value_sum(X) when is_integer(X) -> X.


count_number_of_items(L) ->
    sum([ item_count(X) || X <- L ]).

item_count(#val{n=N}) -> N;
item_count(X) when is_integer(X) -> 1.


analyze_function(AST) ->
    [{complexity, structural_complexity(AST)},
     {expressions_per_function, lines_per_function(AST)},
     {clauses, clauses_per_function(AST)},
     {arity, function_arity(AST)},
     {expressions_per_line, expressions_per_function_line(AST)},
     {variable_steppings, variable_steppings_per_function(AST)}
    ].

warning_metric(Warnings) ->
    {warnings, length(Warnings)}.

expressions_per_function_line({function,_,_,_,Clauses}) -> 
    LNs = [ get_toplevel_linenumbers(C) || C <- Clauses],
    ROSL = repeats_on_same_line(lists:flatten(LNs)),
    #val{max=lists:max(ROSL), min=lists:min(ROSL), sum=sum(ROSL),
	 n=length(ROSL)}.


lines_per_function(AST) ->
    LNs = get_linenumbers(AST),
    length(lists:usort(lists:flatten(LNs))).

is_ast_function(X) when element(1,X) == function -> true;
is_ast_function(_) -> false.

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
    lists:all(fun is_ascii_integer/1, L).

is_ascii_integer(X) when (X>=$0), (X=<$9) -> true; 
is_ascii_integer(_) -> false.


structural_complexity(L) when is_list(L) ->
    sum([ structural_complexity(X) || X <- L ]);
structural_complexity({function, _, _, _, Clauses}) ->
    lists:max([ structural_complexity(X) || X <- Clauses ]);
structural_complexity({clause, _, Match, Guards, Exprs}) ->
    structural_complexity(Match) 
	+ structural_complexity(Guards) 
	+ structural_complexity(Exprs);
structural_complexity({match,_,RHS,LHS}) ->
    1+structural_complexity(RHS) + structural_complexity(LHS);
structural_complexity({call,_, _, Args}) ->
    1+structural_complexity(Args);
structural_complexity({bin,_, Elems}) ->
    1+structural_complexity(Elems);
structural_complexity({bin_element,_, Elem, _, _}) ->
    structural_complexity(Elem);
structural_complexity({'case',_, Expr, Clauses}) ->
    1 + structural_complexity(Expr) + structural_complexity(Clauses);
structural_complexity({'if',_, Clauses}) ->
    1 + structural_complexity(Clauses);
structural_complexity({'receive',_,Clauses}) ->
    1 + structural_complexity(Clauses);
structural_complexity({'receive',_,Clauses, _, AfterExprs}) ->
    1 + structural_complexity(Clauses) + structural_complexity(AfterExprs);
structural_complexity({cons,_,Hd, Tl}) ->
    structural_complexity(Hd) + structural_complexity(Tl);
structural_complexity({record,_,_,Fields}) ->
    1+structural_complexity(Fields);
structural_complexity({record,_,Var,_,RecordField}) ->
    1+structural_complexity(Var)+structural_complexity(RecordField);
structural_complexity({record_field,_,_,Expr}) ->
    1+structural_complexity(Expr);
structural_complexity({record_field,_,Expr1,_,Expr2}) ->
    1+structural_complexity(Expr1)+structural_complexity(Expr2);
structural_complexity({record_index,_,_,Expr}) ->
    1+structural_complexity(Expr);
structural_complexity({tuple,_,Elements}) ->
    1+structural_complexity(Elements);
structural_complexity({op,_,_,LHS,RHS}) ->
    1+structural_complexity(LHS) + structural_complexity(RHS);
structural_complexity({op,_,_,Expr}) ->
    1+structural_complexity(Expr);
structural_complexity({lc,_,Body,Generator}) ->
    1+structural_complexity(Body)+structural_complexity(Generator);
structural_complexity({generate,_,Expr,Guards}) ->
    structural_complexity(Expr) + structural_complexity(Guards);
structural_complexity({'catch',_,CallExpr}) ->
    1+structural_complexity(CallExpr);
structural_complexity({'fun',_,Expr}) ->
    1+structural_complexity(Expr);
structural_complexity({clauses,Clauses}) ->
    0+structural_complexity(Clauses);
structural_complexity({'try',_,CallExprs,_,Exprs,_})->
    1+structural_complexity(CallExprs)+structural_complexity(Exprs);
structural_complexity({block, _, CallExprs}) ->
    1+structural_complexity(CallExprs);

structural_complexity({function,_,_}) -> 0;
structural_complexity({function,_,_,_}) -> 0;
structural_complexity({nil,_}) -> 0;
structural_complexity({atom,_,_}) -> 0;
structural_complexity({var,_,_}) -> 0;
structural_complexity({string,_,_}) -> 0;
structural_complexity({integer,_,_}) -> 0;
structural_complexity({float,_,_}) -> 0;
structural_complexity({char,_,_}) -> 0.

repeats_on_same_line(LNs) ->
    repeats_on_same_line(LNs,hd(LNs),0).

repeats_on_same_line([N|R],N,Counted) ->
    repeats_on_same_line(R,N,Counted+1);
repeats_on_same_line([N|R],_,Counted) ->
    [ Counted | repeats_on_same_line(R,N,1) ];
repeats_on_same_line([],_,Counted) ->
    [ Counted ].


get_toplevel_linenumbers({clause,_Line,_,_,Expressions}) ->
    [element(2,L) || L <- Expressions].


get_linenumbers({function,_Line,_,_,Clauses}) ->    
    [ get_linenumbers(C) || C <- Clauses ];
get_linenumbers({clause,_Line,_,_,Expressions}) ->
    get_linenumbers_body(Expressions).

get_linenumbers_body([]) ->
    [];
get_linenumbers_body([{match,L,LHS,RHS}|R]) ->
    RHSLines = get_linenumbers_body([RHS]),
    LHSLines = get_linenumbers_body([LHS]),
    [L|LHSLines]++RHSLines++get_linenumbers_body(R);
get_linenumbers_body([{'case',L,_,Clauses}|R]) ->
    CaseLines = get_linenumbers_body(Clauses),
    [L|CaseLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'receive', L, Clauses}|R]) ->
    CaseLines = get_linenumbers_body(Clauses),
    [L|CaseLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'receive', L, Clauses, _, AfterExprs}|R]) ->
    CaseLines = get_linenumbers_body(Clauses),
    AfterLines = get_linenumbers_body(AfterExprs),
    [L|CaseLines++AfterLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'call',L,_, Args}|R]) ->
    ArgsLines = get_linenumbers_body(Args),
    [L|ArgsLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{'try',L,CallExprs,_,Exprs,_}|T])->
    CallLines = get_linenumbers_body(CallExprs),
    CatchLines = get_linenumbers_body(Exprs),
    [L|CallLines++CatchLines] ++ get_linenumbers_body(T);
get_linenumbers_body([{clause,L,_,_,Expressions}|R]) ->
    BodyLines = get_linenumbers_body(Expressions),
    [L|BodyLines] ++ get_linenumbers_body(R);
get_linenumbers_body([{nil,L}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{op,L,_,_,_}|R]) ->
    [L|get_linenumbers_body(R)];
get_linenumbers_body([{Marker,LN,_}|T]) when is_atom(Marker) ->
    [LN|get_linenumbers_body(T)];
get_linenumbers_body([{Marker,LN,_,_}|T]) when is_atom(Marker) ->
    [LN|get_linenumbers_body(T)];
get_linenumbers_body([{Marker,LN,_,_,_}|T]) when is_atom(Marker) ->
    [LN|get_linenumbers_body(T)].



%%-----------------------
%% Utilities

sort(L) -> lists:sort(L).

usort(L) -> lists:usort(L).
    
sum(X) -> lists:sum(X).

rev(L) -> lists:reverse(L).

replace_tag(Tag, Value, L) ->
    lists:keystore(Tag, 1, L, {Tag, Value}).

gv(Key, L) ->
    proplists:get_value(Key, L).

