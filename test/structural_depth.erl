

-module(structural_depth).
-compile({parse_transform,ess}).
-export([f1/0, f2/0, f3/0, f4/0, f5/0, f6/0, f7/0, f8/0, f9/0]).



f1() ->
    ok.

f2() ->
    case f() of
	ok -> ok;
	_ -> undefined
    end.

f3() ->
    {1,2,3}.

f4() ->
    {1,2,{1,2}}.

f5() ->
    case f1() of
	{ok, _} -> ok;
	_ -> undefined
    end.

f6() ->
    A = a.

f7() ->
    {ok, A} = f1().

f8() ->
    1+2,
    (1+2)-2*(-1).

f9() ->
    [f7() | f6].




