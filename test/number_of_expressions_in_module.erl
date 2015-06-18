-module(number_of_expressions_in_module).
-compile({parse_transform,ess}).
-export([a/0]).

-define(A,1).
-record(abba,{b,c,d,e,g,h=1,
              l=0}).
-include_lib("eunit/include/eunit.hrl").

a() ->
    something,    
    X = m:f(#abba{b=?A}),
    Y = case X of 
            1 ->
                2;
            2 ->
                1;
            _ ->
                something_else
        end,
    Y.
            
