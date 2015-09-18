
-module(ess_structural_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


base_test() ->
    Res = structural_depth("f() -> 3."),
    ?assertEqual(0, Res).


construct_test() ->
    [{"int",
      fun() ->
	      Res = structural_depth("f() -> [3|f()]."),
	      ?assertEqual(1, Res)
      end}].

match_complex_test() ->
    [{"int",
      fun() ->
	      Res = structural_depth("f() -> [3|f()]=f()."),
	      ?assertEqual(2, Res)
      end}].


structural_depth(Str) ->
    ess_test_lib:call_ess(structural_depth, Str).

