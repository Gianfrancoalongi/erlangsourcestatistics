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
