-module(ess_tests).
-include("ess.hrl").
-include_lib("eunit/include/eunit.hrl").

function_names_shall_be_snaked_cased_test() ->
    Code = ["snake_cased() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(0, Penalty).

bad_function_name_test() ->
    Code = ["camelCased1() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(10, Penalty).

same_bad_function_name_is_counted_only_once_test() ->
    Code = ["camelCased1() -> ok;",
            "camelCased1() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(10, Penalty).

bad_function_names_for_different_functions_test() ->
    Code = ["camelCased1() -> ok.",
            "camelCased2() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(20, Penalty).

variables_shall_be_camel_cased_test() ->
    Code = ["f(CamelCase) -> CamelCase."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(0, Penalty).

more_than_one_bad_variable_names_in_same_function_clause_is_bad_test() ->
    Code = ["f(S_1_b, S_2_b) -> "
            "   S_1_b + S_2_b."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(3, Penalty).

more_than_one_bad_variable_names_in_different_function_clauses_is_bad_test() ->
    Code = ["f(S_1_b) -> S_1_b;"
            "f(S_2_b) -> S_2_b."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(3, Penalty).

function_arity_shall_be_less_than_or_equal_to_three_test() ->
    Code = ["f(A,B,C) -> ok."],
    Penalty = run_tc(arity, Code),
    ?assertEqual(0, Penalty).

functions_with_arity_more_than_three_is_bad_test() ->
    Code = ["f(A,B,C,D) -> ok."],
    Penalty = run_tc(arity, Code),
    ?assertEqual(2, Penalty).

functions_may_have_up_to_four_clauses_test() ->
    Code = ["f() -> ok;",
            "f() -> ok;",
            "f() -> ok;",
            "f() -> ok."],
    Penalty = run_tc(clauses, Code),
    ?assertEqual(0, Penalty).

more_than_four_clauses_is_bad_test() ->
    Code = ["f(a) -> ok;",
            "f(b) -> ok;",
            "f(c) -> ok;",
            "f(d) -> ok;",
            "f(f) -> ok."],
    Penalty = run_tc(clauses, Code),
    ?assertEqual(2, Penalty).

up_to_20_expressions_in_a_function_is_okay_test() ->
    Code = ["f() ->",
            "A = 1,",
            "B = 2,",
            "C = 3,",
            "E = 4,",
            "F = 5,",
            "G = 6,",
            "H = 7,",
            "I = 8,",
            "J = 9,",
            "K = 10,",
            "L = 11,",
            "M = 12,",
            "N = 13,",
            "O = 14,",
            "P = 15,",
            "Q = 16,",
            "R = 17,",
            "S = 18,",
            "T = 19,",
            "U = 20."],
    Penalty = run_tc(expressions_per_function, Code),
    ?assertEqual(0, Penalty).

more_than_20_expressions_in_a_function_is_bad_test() ->
    Code = ["f() ->",
            "A = 1,",
            "B = 2,",
            "C = 3,",
            "E = 4,",
            "F = 5,",
            "G = 6,",
            "H = 7,",
            "I = 8,",
            "J = 9,",
            "K = 10,",
            "L = 11,",
            "M = 12,",
            "N = 13,",
            "O = 14,",
            "P = 15,",
            "Q = 16,",
            "R = 17,",
            "S = 18,",
            "T = 19,",
            "U = 20,",
            "V = 21,",
            "W = 22."],
    Penalty = run_tc(expressions_per_function, Code),
    ?assertEqual(1, Penalty).

one_variable_stepping_is_ok_test() ->
    Code = ["f() ->",
            "A = 1,",
            "A1 = A+1."],
    Penalty = run_tc(variable_steppings, Code),
    ?assertEqual(0, Penalty).

variable_stepping_syntax_bases_on_new_test() ->
    Code = ["f() ->",
            "A = 1,",
            "NewA = A+1,",
            "C=3,",
            "NewC=C+1."],
    Penalty = run_tc(variable_steppings, Code),
    ?assertEqual(3, Penalty).

variable_stepping_syntax_bases_on_int_test() ->
    Code = ["f() ->",
            "A = 1,",
            "A1 = A+1,",
            "A2 = A1+1."],
    Penalty = run_tc(variable_steppings, Code),
    ?assertEqual(3, Penalty).

variable_stepping_syntax_bases_on_new_old_test() ->
    Code = ["f(OldA) ->",
            "A = OldA+1,",
            "NewA = A+1."],
    Penalty = run_tc(variable_steppings, Code),
    ?assertEqual(3, Penalty).
    
export_all_is_penalized_test() ->
    Code = ["-compile(export_all)."],
    Penalty = run_tc(export_all, Code),
    ?assertEqual(10, Penalty).

up_to_three_missing_space_after_comma_is_ok_test() ->
    Code = ["f(A,B,C,D) -> ok."],
    Penalty = run_tc(space_after_comma, Code),
    ?assertEqual(0, Penalty).

beyond_three_missing_space_after_comma_test_() ->
    [{"Function head", 
      fun() ->
              Code = ["f(A,B,C,D,E) -> ok."],
              Penalty = run_tc(space_after_comma, Code),
              ?assertEqual(1, Penalty)
      end},
     {"Tuple in body", 
      fun() ->
              Code = ["f() -> {1,2,3,4,5}."],
              Penalty = run_tc(space_after_comma, Code),
              ?assertEqual(1, Penalty)
      end},
     {"List in body", 
      fun() ->
              Code = ["f() -> [1,2,3,4,5]."],
              Penalty = run_tc(space_after_comma, Code),
              ?assertEqual(1, Penalty)
      end},
     {"In function calls", 
      fun() ->
              Code = ["f() -> m:g(1,2,3,4,5)."],
              Penalty = run_tc(space_after_comma, Code),
              ?assertEqual(1, Penalty)
      end},
     {"pattern matching", 
      fun() ->
              Code = ["f() -> [1,2,3,4,5] = m:g()."],
              Penalty = run_tc(space_after_comma, Code),
              ?assertEqual(1, Penalty)
      end}
    ].

no_compiler_warnings_is_ok_test() ->
    Code = ["f(A) -> A."],
    Penalty = run_tc(warnings, Code),
    ?assertEqual(0, Penalty).

compiler_warnings_test() ->
    Code = ["f(A) -> ok."],
    Penalty = run_tc(warnings, Code),
    ?assertEqual(10, Penalty).


nested_clauses_test_() ->
    [{"cases",
      fun() ->
              Code = ["
                      f(A) -> 
                             case g:t() of
                                 ok -> 
                                     case e:w() of 
                                         ok -> ok 
                                     end; 
                                 _ -> false 
                             end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end},

     {"if nesting",
      fun() ->
              Code = ["
                      f(A) -> 
                             if is_integer(1) -> 
                                     if is_atom(1) -> ok;
                                        true -> false
                                     end;
                                true -> ok
                             end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end},
     {"receive",
      fun() ->
              Code = ["
                      f(A) -> 
                             receive
                                 ok -> 
                                     receive
                                         ok -> ok 
                                     end; 
                                 _ -> false 
                             end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end},
     {"try",
      fun() ->
              Code = ["
                      f(A) -> 
                             try 
                                 g:t(),
                                 try 
                                     e:w()
                                 catch _:_ -> e:w()
                                 end
                             catch _:_ -> 
                                     e:w()
                             end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end},
     {"try of",
      fun() ->
              Code = ["
                      f(A) -> 
                             try g:t() of
                                 ok -> 
                                     try e:w() of
                                         ok -> ok
                                     catch _:_ -> e:w()
                                 end
                             catch _:_ -> 
                                     e:w()
                             end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end},
     {"block",
      fun() ->
              Code = ["
                      f(A) -> 
                           begin
                             begin
                                A = 1
                             end
                           end."],
              Penalty = run_tc(nested_clauses, Code),
              ?assertEqual(5, Penalty)
      end}
    ].

complexity_test_() ->
    [
     {"tuple as function argument",
      fun() ->
              Code = ["f() -> m:f({1, 2, 3, 4})."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"tuple as function argument",
      fun() ->
              Code = ["f() -> "
                      " Key = {1, 2, 3, 4},",
                      " ok = m:f(Key)."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"tuple as function argument 2",
      fun() ->
              Code = ["f() -> "
                      "   m:f({1, 2, 3, 4})."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"tuple as function argument 3",
      fun() ->
              Code = ["f() -> "
                      " ok = m:f({1, 2, 3, 4})."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"unpacking result as tuple",
      fun() ->
              Code = ["f() -> "
                      " {ok, F} = m:f({1, 2, 3, 4})."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"unpacking large tuple result",
      fun() ->
              Code = ["f() -> "
                      " {ok, true, F} = m:f()."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(3, Penalty)
      end},
     {"matching in head",
      fun() ->
              Code = ["f({A,B}) -> A + B."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"variable matching in head",
      fun() ->
              Code = ["f(C = {A, B}) -> element(1,C) + A * B."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"matching in case",
      fun() ->
              Code = ["f() -> 
                             case m:g() of
                                 ok -> ok;
                                 Err={error, _} -> Err 
                             end."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"tuples depth 2 is ok",
      fun() ->
              Code = ["f() -> {{1}, a}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"tuples",
      fun() ->
              Code = ["f() -> {{1, {2}}, a}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(3, Penalty)
      end},
     {"lists are not analysed (definition is recursive)",
      fun() ->
              Code = ["f() -> [[1, [2]], a]."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"functions",
      fun() ->
              Code = ["f() -> m:f(m:g(m:h()))."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(3, Penalty)
      end},
     {"record field assignment",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "f() -> #r1{a = 2}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"record within record",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "-record(r2,{}).",
                      "f() -> #r1{a = #r2{}}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"field access of record within record ok if simple",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "-record(r2,{b}).",
                      "f() -> #r1{a = #r2{b = 3}}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end},
     {"field access of record within record",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "-record(r2,{b}).",
                      "-record(r3,{c}).",
                      "f() -> #r1{a = #r2{b = #r3{}}}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(3, Penalty)
      end},
     {"record field used as record",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "-record(r2,{b}).",
                      "-record(r3,{c}).",
                      "f(A) -> (A#r1.a)#r2{}."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(3, Penalty)
      end},
     {"record field once is ok",
      fun() ->
              Code = ["-record(r1,{a}).",
                      "-record(r2,{b}).",
                      "-record(r3,{c}).",
                      "f(A) -> A#r1.a."],
              Penalty = run_tc(complexity, Code),
              ?assertEqual(0, Penalty)
      end}
    ].

multiple_expressions_on_same_line_test_() ->
    [
     {"arguments on same line is ok",
      fun() ->
              Code = ["f(A, B, C, D) -> 
                             {A, B, C, D}."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(0, Penalty)
      end},
     {"multiple assignments on same line is bad",
      fun() ->
              Code = ["f() -> 
                             A = 1, B = 2."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(3, Penalty)
      end},
     {"multiple function calls on same line is bad",
      fun() ->
              Code = ["f() -> 
                             m:f(), l:k()."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(3, Penalty)
      end},
     {"multiple tuple/list expressions on same line is bad",
      fun() ->
              Code = ["f() -> 
                             {1,2}, [1,2]."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(3, Penalty)
      end},
     {"compact case clause on same line is OK",
      fun() ->
              Code = ["f() -> 
                             case m:f() of 
                                 true -> ok; 
                                 _ -> no 
                             end."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(0, Penalty)
      end}
     ,{"case on same line is bad",
      fun() ->
              Code = ["f() -> 
                             case m:f() of true -> ok; _ -> no end."],
              Penalty = run_tc(expressions_per_line, Code),
              ?assertEqual(7, Penalty)
      end}
    ].
              


%% ---------------------------------------------------------------------------
run_tc(Metric, CodeSnippet) ->
    Path = mk_erlang_file(CodeSnippet),
    Dir = filename:dirname(Path),
    MetricOpt = {metrics, [Metric]},
    Opts = ess:get_options(Dir, [MetricOpt]),
    Tree = ess_engine:dir(Dir, Opts),
    Q = ess_engine:quality(Tree, Opts),
%    io:format(user, "Q: ~p~n", [Q]),
    gv(Metric, Q#tree.quality_penalty).

mk_erlang_file(CodeSnippet) ->
    TmpFile = mk_tmp_file("x.erl"),
    ErlCode = ["-module(x).",
               "-compile(export_all)."]
               ++ CodeSnippet,
    file:write_file(TmpFile, join(ErlCode)),
    TmpFile.

mk_tmp_file(Name) ->
    TmpDir = os:cmd("mktemp -d"),
    filename:join(TmpDir -- "\n", Name).

join(Str) ->
    string:join(Str, "\n").

gv(Key, Proplist) ->
    proplists:get_value(Key, Proplist).
