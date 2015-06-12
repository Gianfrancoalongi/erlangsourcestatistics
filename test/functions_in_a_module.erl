-module(functions_in_a_module).
-compile({parse_transform,ess}).
-export([function_0/0,         
         function_1/1,
         function_2/2]).

function_0() -> [].

function_1(Arg) when is_list(Arg) ->
    Arg;
function_1(X) ->
    X.

function_2(Arg1,Arg2) when is_integer(Arg1) ->
    Arg1 + Arg2;
function_2(Arg1,Arg2) when is_list(Arg1) ->
    Arg1 ++ Arg2;
function_2(Arg1,Arg2) when is_tuple(Arg1) ->
    list_to_tuple(tuple_to_list(Arg1)++tuple_to_list(Arg2)).



    
    
