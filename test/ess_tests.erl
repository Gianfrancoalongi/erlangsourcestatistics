-module(ess_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

number_of_expressions_per_function_test_() ->
    [ fun one/0,
      fun large/0
    ].

one() ->
    AST = str2ast("b() -> this_one."),
    Res = ess:number_of_expressions_for_function(AST),
    ?assertEqual(1, Res).

large() ->
    AST = str2ast(large_func()),
    Res = ess:number_of_expressions_for_function(AST),
    ?assertEqual(17, Res).

expressions_per_line_numbers_test_() ->
    [ {"simple",
       fun() ->
               AST = str2ast("f() -> 25."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({1,1,1}, Res)
       end},
      {"2",
      fun() ->
               AST = str2ast("f() -> 25, 24,\n21."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({2,1,2}, Res)              
       end},
      {"receive",
      fun() ->
               AST = str2ast("f() -> receive hej -> 2+33 end."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({1,1,1}, Res) 
       end}].


structural_depth_test_() ->
    [ make_test_case(X) || X <- structural_depth_test_cases() ].

make_test_case({Label, Input, ExpectedResult, Function}) ->
    {Label,
     fun() ->
             AST = str2ast(Input),
             Res = ess:Function(AST),
             ?assertEqual(ExpectedResult, Res)
      end}.

structural_depth_test_cases() ->
    [{"base int", "f() -> 3.", 0,  structural_depth},
     {"base atom", "f() -> ok.", 0,  structural_depth},
     {"base var", "f() -> A.", 0,  structural_depth},
     {"base string", "f() -> \"hej\".", 0,  structural_depth},
     {"base bin", "f() -> << \"hej\" >>.", 1,  structural_depth},
     {"base bin 2", "f() -> << A, B/binary >>.", 1,  structural_depth},
     {"construct bin 3", "f() -> << (f())/binary >>.", 2, structural_depth},
     {"construct record", "f() -> #state{}.", 1,  structural_depth},
     {"base 2", "f() -> 3, \n4.", 0,  structural_depth},
     {"base 3", "f() -> 3, 4.", 0,  structural_depth},
     {"construct", "f() -> [3|f()].", 1, structural_depth},
     {"unary op", "f() -> - 1.", 1, structural_depth},
     {"binary op", "f() -> 1 + 2 .", 1,  structural_depth},
     {"matching", "f() -> [3|f()]=f().", 3, structural_depth},
     {"binary op function", "f() -> 1 + g(f(1)).", 3, structural_depth},
     {"tuple construct", "f() -> { 1, 2 }.", 1, structural_depth},
     {"tuple matching", "f() -> { 1, 2 } = g().", 3, structural_depth},
     {"list comprehension", "f() -> [ 1 || _ <- []].", 1, structural_depth},
     {"list comprehension", "f() -> [ A || A <- [], is_list(A)].", 2, structural_depth},
     {"records match ", "f(#s{a=A, d=#e{}}) -> ok.", 4, structural_depth},
     {"case clause","f() -> case X of 1 -> 2; 2 -> 1 end.", 1, structural_depth},
     {"case clause 2","f() -> case X of 1 -> 2+1; 2 -> 1 end.", 2, structural_depth},
     {"case clause 3","f() -> case g() of 1 -> 2+1; 2 -> 1 end.", 3, structural_depth},
     {"if clause","f() -> if false -> true; true -> false end.", 1, structural_depth},
     {"receive","f() -> receive 1 -> 2+1; 2 -> 1 end.", 2, structural_depth},
     {"receive after","f() -> receive 1 -> 1 after 3 -> 1+2 end.", 2, structural_depth}
    ].

analyze_function_test() ->
    AST = str2ast("f() -> 1."),
    Res = ess:analyze_function(AST),
    Expected = lists:sort([{arity, 0},
                           {clauses, 1},
                           {depth, 0},
                           {variable_steppings, 0},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, 1}
                          ]),
    ?assertEqual(Expected, Res).

clauses_per_function_test_() ->
    [{"one",
      fun() ->
              AST = str2ast("f() -> 1."),
	      Res = ess:clauses_per_function(AST),
	      ?assertEqual(1, Res)
      end},
     {"three",
     fun() ->
              AST = str2ast("f(_) -> 1;f(_) -> 2;f(_) -> 3."),
	      Res = ess:clauses_per_function(AST),
	      ?assertEqual(3, Res)
     end}].
              

read_erlang_file_test() ->
    Res = ess:file("../test/file_read.erl"),
    Expected = ok,
    ?assertEqual(Expected, Res).

large_func() ->
"a(N) when is_integer(N) ->
    case N of
        2 ->
            something_else,
            case N+1 of
                3 ->
                    what_about,
                    this_line,
                    m:f(\"one\");
                1 ->
                    ok
            end,
            something;
        _ ->
            ignore,
            m:f(),
            b()
    end;
a(X) when is_list(X) ->
   X;
a(_) ->
   k.
".

str2ast(Str) ->
    {ok,FunTkns,_} = erl_scan:string(Str),
    {ok,FunForms} = erl_parse:parse_form(FunTkns),
    FunForms.

no_variable_stepping_test() ->
    AST = str2ast("f() -> A = 0."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(0,Res).

one_variable_stepping_test() ->
    AST = str2ast("f() -> A = 0, A1 = f(A), A1."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(1,Res).
    
case_branch_variable_stepping_test() ->
    AST = str2ast("f() -> A=1, case A of NewA -> NewA end."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(1,Res).

case_clause_variable_stepping_test() ->
    AST = str2ast("f() -> A=1, case A of 2 -> A1 = g(2) end."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(1,Res).

case_clause_and_branch_variable_stepping_test() ->
    AST = str2ast("f() -> A=1, case A of NewA -> NewA2 = g(NewA) end."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(2,Res).

function_argument_variable_stepping_test() ->
    AST = str2ast("f(A,B) -> NewA=1."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(1,Res).

function_record_argument_variable_stepping_test() ->
    AST = str2ast("f(#rec{a=A},B) -> NewA=1."),
    Res = ess:variable_steppings_per_function(AST),
    ?assertEqual(1,Res).

stepping_test_() ->
    [{"no stepping", 
      fun() ->
              Input = ["A", "B"],
              Res = ess:stepping(Input),
              ?assertEqual(0, Res)
      end},
     {"trailing int", 
      fun() ->
              Input = ["A", "A1"],
              Res = ess:stepping(Input),
              ?assertEqual(1, Res)
      end},
     {"trailing 2", 
      fun() ->
              Input = ["A", "A5"],
              Res = ess:stepping(Input),
              ?assertEqual(1, Res)
      end},
     {"trailing 3", 
      fun() ->
              Input = ["A2", "A1543"],
              Res = ess:stepping(Input),
              ?assertEqual(1, Res)
      end},
     {"New...", 
      fun() ->
              Input = ["York", "NewYork"],
              Res = ess:stepping(Input),
              ?assertEqual(1, Res)
      end},
     {"Old...",
      fun() ->
              Input = ["OldYork", "York", "NewYork", "NewYork1"],
              Res = ess:stepping(Input),
              ?assertEqual(3, Res)
      end
     }].


