-module(ess_engine).
-include("ess.hrl").

-export([dir/1,
         dir/2,
         quality/2]).

quality(T = #tree{type=function}, Opts) ->
    RV = T#tree.raw_values,
    QP = calculate_quality_penalty(RV, Opts),

    io:format("  RV-function:~p~n",[RV]),
    io:format("QP-function:~p~n",[QP]),

    T#tree{quality_penalty = QP,
           quality = 100 - lists:sum([V||{_,V}<-QP])
          };
quality(T = #tree{type=file}, Opts) ->
    CS = T#tree.children,
    CS2 = [ quality(C, Opts) || C <- CS],
    CQP = lists:flatten([ C#tree.quality_penalty || C <- CS2 ]),
    RV = T#tree.raw_values,
    QP = calculate_quality_penalty(RV, Opts),
    QP2 = key_sum(QP ++ CQP),

    io:format("  RV-file:~p~n",[RV]),
    io:format("QP-file:~p~n",[QP2]),

    T#tree{children = CS2,
           quality_penalty = QP2,
           quality = 100 - lists:sum([V||{_,V}<-QP2])
          };
quality(T = #tree{type=dir}, Opts) ->
    CS = T#tree.children,
    CS2 = [ quality(C, Opts) || C <- CS],
    CQP = lists:flatten([ C#tree.quality_penalty || C <- CS2 ]),

%%    io:format("QP-dir:~p~n",[CQP]),

    T#tree{children = CS2,
           quality_penalty = key_sum(CQP),
           quality = 100 - lists:sum([V||{_,V}<-CQP])
          }.

key_sum(Proplist) ->
    Keys = lists:usort([K||{K,_}<-Proplist]),
    [ {K, lists:sum(get_all_values(K,Proplist))} || K <- Keys ].

calculate_quality_penalty(RawValues, Opts) ->
    Metrics = gv(metrics, Opts),
    [ penalty_for(M, RawValues) || M <- Metrics ].


-define(MAX_PENALTY, 10).

penalty_for({Key, {Min, Max}}, Values) ->
    Penalty = lists:sum([ penalty(V, Min, Max) ||  {K,V} <- Values, K == Key ]),
    {Key, Penalty}.

penalty(undefined, _, _) -> 0;
penalty(#val{avg=Avg}, Min, Max) ->
    penalty(Avg, Min, Max);

penalty(Val, Min, _) when Val < Min -> 0;
penalty(Val, _, Max) when Val > Max -> ?MAX_PENALTY;
penalty(Val, Min, Max)  ->
    Penalty = ((Val - Min) / (Max - Min)) * ?MAX_PENALTY,
    round(Penalty).

dir(Dir) ->
    dir(Dir, []).
dir(Dir, Opts) ->
    IncDirs = find_hrl_dirs(Dir, Opts),
    add_parse_transform_dir(Opts),
    IncDirOpt = make_inc_compiler_opt(IncDirs),

    reset_log(),
    log("options:~p~n"
        "incdirs:~p~n",[Opts, IncDirOpt]),

    ForEachFileFun = fun(File) -> file(File, Opts, IncDirOpt) end,
    find_files(Dir, ForEachFileFun, Opts).

make_inc_compiler_opt(L) ->
    [{i,IC} || IC <- L ].

add_parse_transform_dir(Opts) ->
    case gv(parse_transform_beam_dirs, Opts) of
        Dirs = [[_|_]|_] ->
            [ add_path(D) || D <- Dirs ];
        Dir = [_|_] ->
            add_path(Dir);
        _ ->
            ok
    end.

add_path(Path) ->
    code:add_patha(Path).

find_hrl_dirs(Dir, Opts) ->
    BlackList = gv(exclude_dir_patterns, Opts, []) ++
                gv(exclude_dir_patterns_during_hrl_search, Opts, []),
    case gv(include_paths, Opts, []) of
        [] ->
            find_dirs(".hrl", Dir, BlackList);
        Paths ->
            find_in_subdirs(".hrl", Paths, BlackList)
    end.

find_dirs(Ext, Dir, BlackList) ->
    Fs = list_dir_full_names(Dir),
    HasFiles = any_file_has_extension(Ext, Fs),
    WhiteDirs = remove_blacklisted(BlackList, Fs),
    SubDirs = find_in_subdirs(Ext, WhiteDirs, BlackList),
    if HasFiles -> [Dir | SubDirs];
       true -> SubDirs
    end.

find_in_subdirs(Ext, Dirs, BlackList) ->
    lists:concat([ find_dirs(Ext, D, BlackList) || D <- Dirs ]).

any_file_has_extension(Ext, Fs) ->
    lists:any(fun(F) -> filename:extension(F) == Ext end, Fs).

remove_blacklisted(BlackList, Fs) ->
    lists:filter(fun(F) -> not is_blacklisted(BlackList, F) end, Fs).

is_blacklisted(BlackList, F) ->
    lists:any(fun(B) -> is_string_in_name(F, B) end, BlackList).

is_erlang_source_file(F) ->
    filename:extension(F) == ".erl".

files_ending_in_erl(Fs) ->
    lists:filter(fun is_erlang_source_file/1, Fs).

list_dir_full_names(Dir) ->
    case file:list_dir(Dir) of
        {ok, Fs} -> mk_fullnames(Dir, Fs);
        _ -> []
    end.

mk_fullnames(Dir, Fs) ->
    [ filename:join(Dir, F) || F <- Fs ].

is_string_in_name(Name, String) ->
    string:str(Name, String) /= 0.


find_files(Dir, ForEachFileFun, Opts) ->
    BlackList = gv(exclude_dir_patterns_during_analysis, Opts, []) ++
                gv(exclude_dir_patterns, Opts, []),
    find_files2(Dir, BlackList, ForEachFileFun).

find_files2(Dir, BlackList, ForEachFileFun) ->
    Fs = list_dir_full_names(Dir),
    WhiteFs = remove_blacklisted(BlackList, Fs),
    SrcFiles = files_ending_in_erl(WhiteFs),
    SubDirs = find_in_subdirs_par(WhiteFs, BlackList, ForEachFileFun),
    if (SrcFiles/=[]) andalso (SubDirs/=[]) ->
            io:format("Warning, dir contains both source files and dirs: ~p~n",
                      [Dir]);
       true -> ok
    end,
    Stats = for_each_file_par(SrcFiles, ForEachFileFun) ++ SubDirs,
    #tree{type = dir,
          name = Dir,
          children = Stats}.

find_in_subdirs_par(Dirs, BlackList, ForEachFileFun) ->
    Fun = fun(D) -> find_files2(D, BlackList, ForEachFileFun) end,
    RecData = run_fun_async(Dirs, Fun),
    prune_empties(receive_answers(RecData)).

prune_empties(L) ->
    lists:filter(fun is_not_empty/1, L).

is_not_empty({_, [], []}) -> false;
is_not_empty(_) -> true.

for_each_file_par(Files, Fun) ->
    RecData = run_fun_async(Files, Fun),
    receive_answers(RecData).

run_fun_async(Fs, Fun) ->
    [run_one_async(F, Fun) || F <- Fs].

run_one_async(F, Fun) ->
    Me = self(),
    {spawn(fun() ->
                   Res = (catch Fun(F)),
                   Me ! {self(), Res}
           end), F}.

receive_answers(L) ->
    [receive {Pid, Res} -> Res after 150000 -> {timeout, F} end
     || {Pid, F} <- L].


file(F, Opts, IncPaths) ->
    try
        CompileOpts = get_compile_options(),
        {ok,Mod,Bin,Warnings} = compile:file(F,CompileOpts ++ IncPaths),
        {ok,{Mod,[{abstract_code,{raw_abstract_v1,AST}}]}} =
            beam_lib:chunks(Bin,[abstract_code]),
        RawValues = file_raw_values(AST, Warnings, F, Opts),        
        RawChildren = analyse_functions(AST, Opts),
        io:format("  f: ~s: ok~n", [F]),
        #tree{type = file,
              name = F,
              raw_values = RawValues,
              children = RawChildren
             }
    catch
        _:Err ->
            io:format("  f: ~s: error: ~p~n", [F, error_digest(Err)]),
            log("f: ~p ~p ~p~n",[F, Err, erlang:get_stacktrace()]),
            undefined
    end.

error_digest({badmatch, {error, [{_, [{_,epp, {include, file, IncFile}}|_]}|_],_}
             }) ->
    "missing_include: "++IncFile;
error_digest(_) ->
    "error".

reset_log() ->
    file:delete("/tmp/ess_errors.log").

log(Fmt, Args) ->
    Message = io_lib:format(Fmt, Args),
    file:write_file("/tmp/ess_errors.log", Message, [append]).

get_all_values(K, Proplist) ->
    [ V || {Key,V} <- Proplist, Key == K ].

get_compile_options() ->
    [binary,verbose, debug_info, return].

lexical_analyse(F, _Opts) ->
    {ok, Bin} = file:read_file(F),
    lexical_analyse_string(binary_to_list(Bin)).

lexical_analyse_string(Str) ->
    Lines = strip_lines(divide_into_lines(Str)),
    seq_accum([], Lines,
              [fun count_comment_and_code_lines/1,
               fun analyse_space_after_comma/1]).

count_comment_and_code_lines(L) ->
    Tot = length(L),
    LineLengths = line_lengths(L),
    {Code, Comment, Blank} = count_comment_and_code_lines2(L, 0, 0, 0),
    Values = [{total_lines, Tot},
              {lines_of_code, Code},
              {lines_of_comments, Comment},
              {line_lengths, LineLengths},
              {blank_lines, Blank}],
    handle_comment_percent(Values).

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

analyse_space_after_comma(Ls) ->
    Faults = sum([ sac(L) || L <- Ls, not is_comment_line(L) ]),
    [{space_after_comma, Faults}].

sac([$,,$ | L]) ->
    sac(L);
sac([$,]) ->
    0;
sac([$,| L]) ->
    1+sac(L);
sac([_| L]) ->
    sac(L);
sac([]) ->
    0.

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
    [string:strip(L) || L <- Ls ].

file_raw_values(AST, Warnings, F, Opts) ->
    EA = export_all_metric(AST),
    WM = warning_metric(Warnings),
    LA = lexical_analyse(F, Opts),
    [ EA, WM | LA ].

export_all_metric([]) ->
    {export_all, 0};
export_all_metric([{attribute, _, compile, export_all}|_]) ->
    {export_all, 1};
export_all_metric([_|T]) ->
    export_all_metric(T).

analyse_functions(AST, _Opts) ->
    [ analyze_function(F) || F <- AST, is_ast_function(F) ].

handle_comment_percent(L) ->
    Percent = calculate_comment_to_line_percent(L),
    replace_tag(comment_to_line_percent, Percent, L).

calculate_comment_to_line_percent(L) ->
    Lines = value_sum(gv(total_lines,L)),
    Comments = value_sum(gv(lines_of_comments,L)),
    round(100*(Comments/Lines)).

value_sum(#val{sum=Sum}) -> Sum;
value_sum(X) when is_integer(X) -> X.

analyze_function(AST={function, _, _Name, _, _}) ->
    #tree{type = function,
          name = make_name(AST),
          raw_values = [{function_naming, function_naming(AST, false)},
                        {variable_naming, variable_naming(AST, false)},
                        {nested_clauses, nested_clauses(AST)},
                        {complexity, complexity(AST)},
                        {expressions_per_function, lines_per_function(AST)},
                        {clauses, clauses_per_function(AST)},
                        {arity, function_arity(AST)},
                        {expressions_per_line, expressions_per_function_line(AST)},
                        {variable_steppings, variable_steppings_per_function(AST)}
                       ]}.

function_naming({function, _, Name, _, _Clauses}, _FromMatch) ->
    snake_case(Name).

variable_naming({function, _, _Name, _, Clauses}, FromMatch) ->
    variable_naming(Clauses, FromMatch);
variable_naming(L, FromMatch) when is_list(L) ->
    sum([variable_naming(X, FromMatch) || X <- L]);
variable_naming({clause, _, Match, Guards, Exprs}, FromMatch) ->
    sum([variable_naming(Match, true),
         variable_naming(Guards, FromMatch),
         variable_naming(Exprs, FromMatch)]);
variable_naming({match,_,LHS, _RHS}, FromMatch) ->
    sum(variable_naming(LHS, true),
        variable_naming(LHS, FromMatch));
variable_naming({call,_, _, Args}, FromMatch) ->
    variable_naming(Args, FromMatch);
variable_naming({bin,_, Elems}, FromMatch) ->
    variable_naming(Elems, FromMatch);
variable_naming({bin_element,_, Elem, _, _}, FromMatch) ->
    variable_naming(Elem, FromMatch);
variable_naming({'case',_, Expr, Clauses}, FromMatch) ->
    sum(variable_naming(Expr, FromMatch),
        variable_naming(Clauses, FromMatch));
variable_naming({'if',_, Clauses}, FromMatch) ->
    variable_naming(Clauses, FromMatch);
variable_naming({'receive',_,Clauses}, FromMatch) ->
    variable_naming(Clauses, FromMatch);
variable_naming({'receive',_,Clauses, _, AfterExprs}, FromMatch) ->
    sum(variable_naming(Clauses, FromMatch),
        variable_naming(AfterExprs, FromMatch));
variable_naming({cons,_,Hd, Tl}, FromMatch) ->
    sum(variable_naming(Hd, FromMatch) ,
        variable_naming(Tl, FromMatch));
variable_naming({record,_,_,Fields}, FromMatch) ->
    variable_naming(Fields, FromMatch);
variable_naming({record,_,Var,_,RecordField}, FromMatch) ->
    sum(variable_naming(Var, FromMatch),
        variable_naming(RecordField, FromMatch));
variable_naming({record_field,_,_,Expr}, FromMatch) ->
    variable_naming(Expr, FromMatch);
variable_naming({record_field,_,Expr1,_,Expr2}, FromMatch) ->
    sum(variable_naming(Expr1, FromMatch),
        variable_naming(Expr2, FromMatch));
variable_naming({record_index,_,_, Expr}, FromMatch) ->
    variable_naming(Expr, FromMatch);
variable_naming({tuple,_,Elements}, FromMatch) ->
    variable_naming(Elements, FromMatch);
variable_naming({op,_,_,LHS,RHS}, FromMatch) ->
    sum(variable_naming(LHS, FromMatch),
        variable_naming(RHS, FromMatch));
variable_naming({op,_,_,Expr}, FromMatch) ->
    variable_naming(Expr, FromMatch);
variable_naming({lc,_,Body,Generator}, FromMatch) ->
    sum(variable_naming(Body, FromMatch),
        variable_naming(Generator, FromMatch));
variable_naming({generate,_,Expr,Guards}, FromMatch) ->
    sum(variable_naming(Expr, FromMatch),
        variable_naming(Guards, FromMatch));
variable_naming({b_generate,_,Expr,Guards}, FromMatch) ->
    sum(variable_naming(Expr, FromMatch),
        variable_naming(Guards, FromMatch));
variable_naming({'catch',_,CallExpr}, FromMatch) ->
    variable_naming(CallExpr, FromMatch);
variable_naming({'fun',_,Expr}, FromMatch) ->
    variable_naming(Expr, FromMatch);
variable_naming({clauses,Clauses}, FromMatch) ->
    variable_naming(Clauses, FromMatch);
variable_naming({'try',_,CallExprs,_,Exprs,_}, FromMatch)->
    sum(variable_naming(CallExprs, FromMatch),
        variable_naming(Exprs, FromMatch));
variable_naming({block, _, CallExprs}, FromMatch) ->
    variable_naming(CallExprs, FromMatch);
variable_naming({bc,_,Body,Generator}, FromMatch) ->
    sum(variable_naming(Body, FromMatch),
        variable_naming(Generator, FromMatch));

variable_naming({var,_,V}, true) -> camel_case(V);
variable_naming({atom,_,A}, _) -> snake_case(A);

variable_naming({function,_,_}, _) -> 0;
variable_naming({function,_,_,_}, _) -> 0;
variable_naming({nil,_}, _) -> 0;
variable_naming({var,_,_}, _) -> 0;
variable_naming({string,_,_}, _) -> 0;
variable_naming({integer,_,_}, _) -> 0;
variable_naming({float,_,_}, _) -> 0;
variable_naming({char,_,_}, _) -> 0.


snake_case(Input) ->
    case is_snake_cased(to_string(Input)) of
        true -> 0;
        _ -> 1
    end.

is_snake_cased(String) ->
    OnlyLowerCase = string:to_lower(String) == String,
    HasUnderscore = lists:member($_, String),
    IMN = is_module_name(String),
    OnlyLowerCase or HasUnderscore or IMN.

is_module_name(String) ->
    index_of_first_uppercase(String) =< 4.

index_of_first_uppercase(S) ->
    index_of_first_uppercase(1, S).

index_of_first_uppercase(_, []) ->
    0;
index_of_first_uppercase(Ix, [C|_]) when C =< $Z, C >= $A ->
    Ix;
index_of_first_uppercase(Ix, [_|Cs]) ->
    index_of_first_uppercase(Ix+1, Cs).

camel_case(Input) ->
    case is_camel_cased(to_string(Input)) of
        true -> 0;
        _ ->  1
    end.

is_camel_cased([$_|_]) ->
    true;
is_camel_cased(String) when length(String) > 3 ->
    HasUpperCase = string:to_lower(String) /= String,
    HasLowerCase = string:to_upper(String) /= String,
    HasUnderscore = lists:member($_, String),
    HasUpperCase andalso HasLowerCase andalso not HasUnderscore;
is_camel_cased(_) ->
    true.

to_string(X) when is_atom(X) -> atom_to_list(X);
to_string(X) when is_list(X) -> X.

make_name({function, _, Name, Arity, _}) ->
    list_to_atom(lists:flatten(io_lib:format("~p|~p",[Name, Arity]))).

warning_metric(Warnings) ->
    {warnings, length(Warnings)}.

expressions_per_function_line({function,_,_,_,Clauses}) ->
    LNs = [ get_toplevel_linenumbers(C) || C <- Clauses],
    ROSL = repeats_on_same_line(lists:flatten(LNs)),
    calc_avg(#val{max=lists:max(ROSL),
                  min=lists:min(ROSL),
                  sum=sum(ROSL),
                  n=length(ROSL)}).

calc_avg(V=#val{n=0}) ->
    V#val{avg = 0};
calc_avg(V=#val{n=N, sum=Sum}) ->
    V#val{avg = round(Sum / N)}.

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


complexity(L) when is_list(L) ->
    max([ complexity(X) || X <- L ]);
complexity({function, _, _, _, Clauses}) ->
    max([ complexity(X) || X <- Clauses ]);
complexity({clause, _, Match, Guards, Exprs}) ->
    max([complexity(Match),
         complexity(Guards),
         complexity(Exprs)]);
complexity({match,_,RHS,LHS}) ->
    1 + max(complexity(RHS) , complexity(LHS));
complexity({call,_, _, Args}) ->
    1+complexity(Args);
complexity({bin,_, Elems}) ->
    1+complexity(Elems);
complexity({bin_element,_, Elem, _, _}) ->
    complexity(Elem);
complexity({'case',_, Expr, Clauses}) ->
    1 + max(complexity(Expr), complexity(Clauses));
complexity({'if',_, Clauses}) ->
    1 + complexity(Clauses);
complexity({'receive',_,Clauses}) ->
    1 + complexity(Clauses);
complexity({'receive',_,Clauses, _, AfterExprs}) ->
    1 + max(complexity(Clauses), complexity(AfterExprs));
complexity({cons,_,Hd, Tl}) ->
    max(complexity(Hd) , complexity(Tl));
complexity({record,_,_,Fields}) ->
    1+complexity(Fields);
complexity({record,_,Var,_,RecordField}) ->
    1+max(complexity(Var), complexity(RecordField));
complexity({record_field,_,_,Expr}) ->
    complexity(Expr);
complexity({record_field,_,Expr1,_,Expr2}) ->
    1 + (1+complexity(Expr1)) + complexity(Expr2);
complexity({record_index,_,_,Expr}) ->
    1+complexity(Expr);
complexity({tuple,_,Elements}) ->
    1+complexity(Elements);
complexity({op,_,_,LHS,RHS}) ->
    1+ max(complexity(LHS), complexity(RHS));
complexity({op,_,_,Expr}) ->
    1+complexity(Expr);
complexity({lc,_,Body,Generator}) ->
    1+ max(complexity(Body), complexity(Generator));
complexity({generate,_,Expr,Guards}) ->
    max(complexity(Expr), complexity(Guards));
complexity({b_generate,_,Expr,Guards}) ->
    max(complexity(Expr), complexity(Guards));
complexity({'catch',_,CallExpr}) ->
    1+complexity(CallExpr);
complexity({'fun',_,Expr}) ->
    1+complexity(Expr);
complexity({clauses,Clauses}) ->
    0+complexity(Clauses);
complexity({'try',_,CallExprs,_,Exprs,_})->
    1+ max(complexity(CallExprs), complexity(Exprs));
complexity({block, _, CallExprs}) ->
    1+complexity(CallExprs);
complexity({bc,_,Body,Generator}) ->
    1+ max(complexity(Body), complexity(Generator));

complexity({function,_,_}) -> 0;
complexity({function,_,_,_}) -> 0;
complexity({nil,_}) -> 0;
complexity({atom,_,_}) -> 0;
complexity({var,_,_}) -> 0;
complexity({string,_,_}) -> 0;
complexity({integer,_,_}) -> 0;
complexity({float,_,_}) -> 0;
complexity({char,_,_}) -> 0.

%% --------------------------------------------------

nested_clauses(L) ->
    nested_clauses(L, 0).

nested_clauses({function, _, _, _, Clauses}, ClauseDepth) ->
    max([ nested_clauses(X, ClauseDepth) || X <- Clauses ]);
nested_clauses(L, ClauseDepth) when is_list(L) ->
    max([ nested_clauses(X, ClauseDepth) || X <- L ]);
nested_clauses({clause, _, Match, Guards, Exprs}, ClauseDepth) ->
    max([nested_clauses(Match, ClauseDepth),
         nested_clauses(Guards, ClauseDepth),
         nested_clauses(Exprs, ClauseDepth)]);
nested_clauses({match,_,RHS,LHS}, ClauseDepth) ->
    max(nested_clauses(RHS, ClauseDepth),
        nested_clauses(LHS, ClauseDepth));
nested_clauses({call,_, _, Args}, ClauseDepth) ->
    nested_clauses(Args, ClauseDepth);
nested_clauses({bin,_, Elems}, ClauseDepth) ->
    nested_clauses(Elems, ClauseDepth);
nested_clauses({bin_element,_, Elem, _, _}, ClauseDepth) ->
    nested_clauses(Elem, ClauseDepth);
nested_clauses({'case',_, Expr, Clauses}, ClauseDepth) ->
    NewClauseDepth = 1 + ClauseDepth,
    max(nested_clauses(Expr, NewClauseDepth), 
        nested_clauses(Clauses, NewClauseDepth));
nested_clauses({'if', _, Clauses}, ClauseDepth) ->
    nested_clauses(Clauses, ClauseDepth + 1);
nested_clauses({'receive', _, Clauses}, ClauseDepth) ->
    nested_clauses(Clauses, ClauseDepth + 1);
nested_clauses({'receive',_,Clauses, _, AfterExprs}, ClauseDepth) ->
    NewClauseDepth = ClauseDepth + 1,
    max(nested_clauses(Clauses, NewClauseDepth), 
        nested_clauses(AfterExprs, NewClauseDepth));
nested_clauses({cons,_,Hd, Tl}, ClauseDepth) ->
    max(nested_clauses(Hd, ClauseDepth),
        nested_clauses(Tl, ClauseDepth));
nested_clauses({record,_,_,Fields}, ClauseDepth) ->
    nested_clauses(Fields, ClauseDepth);
nested_clauses({record,_,Var,_,RecordField}, ClauseDepth) ->
    max(nested_clauses(Var, ClauseDepth), 
        nested_clauses(RecordField, ClauseDepth));
nested_clauses({record_field,_,_,Expr}, ClauseDepth) ->
    nested_clauses(Expr, ClauseDepth);
nested_clauses({record_field,_,Expr1,_,Expr2}, ClauseDepth) ->
    max(nested_clauses(Expr1, ClauseDepth), 
        nested_clauses(Expr2, ClauseDepth));
nested_clauses({record_index,_,_,Expr}, ClauseDepth) ->
    nested_clauses(Expr, ClauseDepth);
nested_clauses({tuple,_,Elements}, ClauseDepth) ->
    nested_clauses(Elements, ClauseDepth);
nested_clauses({op, _, _, LHS, RHS}, ClauseDepth) ->
    max(nested_clauses(LHS, ClauseDepth),
        nested_clauses(RHS, ClauseDepth));
nested_clauses({op, _, _, Expr}, ClauseDepth) ->
    nested_clauses(Expr, ClauseDepth);
nested_clauses({lc, _, Body, Generator}, ClauseDepth) ->
    max(nested_clauses(Body, ClauseDepth),
        nested_clauses(Generator, ClauseDepth));
nested_clauses({generate, _, Expr, Guards}, ClauseDepth) ->
    max(nested_clauses(Expr, ClauseDepth), 
        nested_clauses(Guards, ClauseDepth));
nested_clauses({b_generate, _, Expr, Guards}, ClauseDepth) ->
    max(nested_clauses(Expr, ClauseDepth), 
        nested_clauses(Guards, ClauseDepth));
nested_clauses({'catch', _, CallExpr}, ClauseDepth) ->
    nested_clauses(CallExpr, ClauseDepth);
nested_clauses({'fun', _, Expr}, ClauseDepth) ->
    nested_clauses(Expr, ClauseDepth);
nested_clauses({clauses,Clauses}, ClauseDepth) ->
    nested_clauses(Clauses, ClauseDepth);
nested_clauses({'try', _, CallExprs, Clauses, CatchClauses,_}, ClauseDepth)->
    NewClauseDepth = ClauseDepth + 1,
    max([nested_clauses(CallExprs, NewClauseDepth), 
         nested_clauses(Clauses, NewClauseDepth),
         nested_clauses(CatchClauses, NewClauseDepth)]);
nested_clauses({block, _, CallExprs}, ClauseDepth) ->
    nested_clauses(CallExprs, ClauseDepth +1 );
nested_clauses({bc,_,Body,Generator}, ClauseDepth) ->
    max(nested_clauses(Body, ClauseDepth), 
        nested_clauses(Generator, ClauseDepth));

nested_clauses({function, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({function, _, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({nil, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({atom, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({var, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({string, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({integer, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({float, _, _}, ClauseDepth) -> ClauseDepth;
nested_clauses({char, _, _}, ClauseDepth) -> ClauseDepth.

%% ------------------------------------------------------------


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

usort(L) -> lists:usort(L).

sum(A, B) -> A+B.
sum(L) -> lists:sum(L).

max([]) -> 0;
max(L) -> lists:max(L).

rev(L) -> lists:reverse(L).

replace_tag(Tag, Value, L) ->
    lists:keystore(Tag, 1, L, {Tag, Value}).

gv(Key, L) ->
    proplists:get_value(Key, L).
gv(Key, L, Def) ->
    proplists:get_value(Key, L, Def).


seq_accum(Acc, A, [F|L]) ->
    Acc2 = F(A) ++ Acc,
    seq_accum(Acc2, A, L);
seq_accum(Acc, _, []) ->
    Acc.
