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
    ?assertEqual(100, Penalty).

same_bad_function_name_is_counted_only_once_test() ->
    Code = ["camelCased1() -> ok;",
            "camelCased1() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(100, Penalty).

bad_function_names_for_different_functions_test() ->
    Code = ["camelCased1() -> ok.",
            "camelCased2() -> ok."],
    Penalty = run_tc(function_naming, Code),
    ?assertEqual(200, Penalty).

variables_shall_be_camel_cased_test() ->
    Code = ["f(CamelCase) -> CamelCase."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(0, Penalty).

more_than_one_bad_variable_names_in_same_function_clause_is_bad_test() ->
    Code = ["f(S_1_b, S_2_b) -> "
            "   S_1_b + S_2_b."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(25, Penalty).

more_than_one_bad_variable_names_in_different_function_clauses_is_bad_test() ->
    Code = ["f(S_1_b) -> S_1_b;"
            "f(S_2_b) -> S_2_b."],
    Penalty = run_tc(variable_naming, Code),
    ?assertEqual(25, Penalty).




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
