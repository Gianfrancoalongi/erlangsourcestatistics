-module(ess_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

lines_per_function_test_() ->
    [ {"receive",
       fun() ->
               AST = str2ast("f() -> receive _ -> 3+3, A=g(), 25 end."),
               Res = ess:lines_per_function(AST),
               ?assertEqual(1, Res)
       end},
      {"one", 
       fun() ->
               AST = str2ast("b() -> this_one."),
               Res = ess:lines_per_function(AST),
               ?assertEqual(1, Res)
       end},
      {"nil", 
       fun() ->
               AST = str2ast("b() -> []."),
               Res = ess:lines_per_function(AST),
               ?assertEqual(1, Res)
       end},
      {"large",
       fun() ->
               AST = str2ast(large_func()),
               Res = ess:lines_per_function(AST),
               ?assertEqual(17, Res)
       end}
    ].

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
       end},
      {"receive with after",
       fun() ->
               AST = str2ast("f() -> receive hej -> 2+33 "
                             "after 120 -> not_ok end."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({1,1,1}, Res) 
       end},
      {"match",
       fun() ->
               AST = str2ast("f() -> A = 1."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({1,1,1}, Res) 
       end},
       {"try",
        fun() ->
               AST = str2ast("f() -> try a() catch O ->done end."),
               Res = ess:expressions_per_function_line(AST),
               ?assertEqual({1,1,1}, Res) 
        end}
     ].

structural_complexity_test_() ->
    [ make_test_case(X) || X <- structural_complexity_test_cases() ].

make_test_case({Label, Input, ExpectedResult, Function}) ->
    {Label,
     fun() ->
             AST = str2ast(Input),
             Res = ess:Function(AST),
             ?assertEqual(ExpectedResult, Res)
      end}.

structural_complexity_test_cases() ->
    [{"base int", "f() -> 3.", 0,  structural_complexity},
     {"base atom", "f() -> ok.", 0,  structural_complexity},
     {"base var", "f() -> A.", 0,  structural_complexity},
     {"base string", "f() -> \"hej\".", 0,  structural_complexity},
     {"base bin", "f() -> << \"hej\" >>.", 1,  structural_complexity},
     {"base bin 2", "f() -> << A, B/binary >>.", 1,  structural_complexity},
     {"construct bin 3", "f() -> << (f())/binary >>.", 2, structural_complexity},
     {"construct record", "f() -> #state{}.", 1,  structural_complexity},
     {"base 2", "f() -> 3, \n4.", 0,  structural_complexity},
     {"base 3", "f() -> 3, 4.", 0,  structural_complexity},
     {"construct", "f() -> [3|f()].", 1, structural_complexity},
     {"unary op", "f() -> - 1.", 1, structural_complexity},
     {"binary op", "f() -> 1 + 2 .", 1,  structural_complexity},
     {"matching", "f() -> [3|f()]=f().", 3, structural_complexity},
     {"binary op function", "f() -> 1 + g(f(1)).", 3, structural_complexity},
     {"tuple construct", "f() -> { 1, 2 }.", 1, structural_complexity},
     {"tuple matching", "f() -> { 1, 2 } = g().", 3, structural_complexity},
     {"list comprehension", "f() -> [ 1 || _ <- []].", 1, structural_complexity},
     {"list comprehension", "f() -> [ A || A <- [], is_list(A)].", 2, structural_complexity},
     {"records match ", "f(#s{a=A, d=#e{}}) -> ok.", 4, structural_complexity},
     {"case clause","f() -> case X of 1 -> 2; 2 -> 1 end.", 1, structural_complexity},
     {"case clause 2","f() -> case X of 1 -> 2+1; 2 -> 1 end.", 2, structural_complexity},
     {"case clause 3","f() -> case g() of 1 -> 2+1; 2 -> 1 end.", 3, structural_complexity},
     {"if clause","f() -> if false -> true; true -> false end.", 1, structural_complexity},
     {"receive","f() -> receive 1 -> 2+1; 2 -> 1 end.", 2, structural_complexity},
     {"receive after","f() -> receive 1 -> 1 after 3 -> 1+2 end.", 2, structural_complexity},
     {"record","f(C#s{a=undefined}) -> ok.",2, structural_complexity},
     {"record_nest", "f(C#s.s1) -> ok.", 1, structural_complexity},
     {"record_index","f(#regC.eri, O) ->ok.",1, structural_complexity},
     {"catch","f()-> case catch a:b(C) of ok -> 1 end.",3,structural_complexity},
     {"fun","f(fun(C) -> a(C) end) -> ok.",2,structural_complexity},
     {"try","f() ->try a() catch O -> done end.",3,structural_complexity}
    ].

analyze_function_test() ->
    AST = str2ast("f() -> 1."),
    Res = ess:analyze_function(AST),
    Expected = lists:sort([{arity, 0},
                           {clauses, 1},
                           {complexity, 0},
                           {variable_steppings, 0},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, 1}
                          ]),
    ?assertEqual(Expected, Res).

analyze_function_with_several_clauses_test() ->
    AST = str2ast("f(1) -> 1;\n"
                  "f(2) -> 2."),
    Res = ess:analyze_function(AST),
    Expected = lists:sort([{arity, 1},
                           {clauses, 2},
                           {complexity, 0},
                           {variable_steppings, 0},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, 2}
                          ]),
    ?assertEqual(Expected, Res).

analyze_big_function_with_three_clauses_test() ->
    AST = str2ast("f(OldA,B,C,D) -> \n"
                  "   A = g(OldA),\n"
                  "   NewA = f(A,B),\n"
                  "   case g(NewA) of\n"
                  "      undefined -> \n"
                  "           ok;\n"
                  "      NewA1 -> \n"
                  "           g(NewA1)\n"
                  "   end;\n"
                  "f(_,B,_,_) -> \n"
                  "   B1 = g(B),\n"
                  "   B2 = g(B1),\n"
                  "   g(B2,B1);\n"
                  "f(_,_,_,_) -> \n"
                  "   ok.\n"),
    Res = ess:analyze_function(AST),
    Expected = lists:sort([{arity, 4},
                           {clauses, 3},
                           {complexity, 7},
                           {variable_steppings, 5},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, 11}
                           ]),
    ?assertEqual(Expected, Res).

analyze_function_with_recieve_after_test() ->
    AST = str2ast("f(OldA) -> \n"
                  "   A = g(OldA),\n"
                  "   receive\n" 
                  "      A -> \n"
                  "           ok\n" 
                  "   after 120 -> \n"
                  "           g(A)\n"
                  "   end;\n"
                  "f(_) -> \n"
                  "   g().\n"),
    Res = ess:analyze_function(AST),
    Expected = lists:sort([{arity, 1},
                           {clauses, 2},
                           {complexity, 4},
                           {variable_steppings, 1},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, 6}
                           ]),
    ?assertEqual(Expected, Res).

analyze_simple_module_test() ->
    Res = ess:file("../test/file_read_test.erl"),
    Expected = lists:sort([{arity, {0,0,0}},
                           {clauses, {1,1,1}},
                           {complexity, {2,1,1}},
                           {variable_steppings, {0,0,0}},
                           {expressions_per_line, {1,1,1}},
                           {expressions_per_function, {2,1,1}}
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

get_compile_include_path_test() ->
    Res = ess:get_compile_include_path("../test/sbg_inc.conf"),
    L = [{i, "/local/scratch/ejunyin/proj/sgc/src/sgc/reg/include"},
         {i, "/local/scratch/ejunyin/proj/sgc/src/syf/ccpc/include/"},
         {i, "/local/scratch/ejunyin/proj/sgc/src/syf/sys/sys_erl/include/"},
         {i, "/local/scratch/ejunyin/proj/sgc/src/syf/sip/include/"}],
    ?assertEqual(L, Res).   
    
get_all_files_test() ->
    Res = ess:get_all_files("../src/"),
    L = ["../src/ess.erl"],
    ?assertEqual(L, Res).
