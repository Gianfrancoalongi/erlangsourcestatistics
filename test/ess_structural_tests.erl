
-module(ess_structural_tests).


-compile(export_all).


base_test() ->
    [{"int",
      fun() ->
	      Res = structural_depth("f() -> 3."),
	      ?assertEqual(0, Res)
      end}].

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
    %% make into aproper erlang module
    Str2 = "-module(xx)\n-export([f/0]).\n"++Str++"\n",
    ess:structural_depth(make_str_into_ast(Str2)).


make_str_into_ast(Str) ->
    ok.
