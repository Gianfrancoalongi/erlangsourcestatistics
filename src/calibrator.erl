-module(calibrator).
-export([run/1,
         run_dir/0]).
-include("ess.hrl").

run(File) ->
    Tree = ess:file(File, [], []),
    R = recalculate_quality(Tree),
    io:format("~p ==> ~p~n ~p~n",[R#tree.name,
                                  R#tree.quality,
                                  R#tree.children]).

run_dir() ->
    RootDir = "/local/scratch/etxpell/proj/erlangsourcestatistics/calibration",
    SGC = ess:dir(RootDir, []),
    R = recalculate_quality(SGC),
    print_tree(R, 0).


print_tree(R, D) ->
    P = lists:duplicate(D, $ ),
    io:format("~s~p:==> ~p (~p)~n",[P,
                                    filename:basename(R#tree.name),
                                    R#tree.quality,
                                    R#tree.quality_penalty]),
    [ print_tree(C, D+4) || C <- R#tree.children].

recalculate_quality(T=#tree{}) ->
    ess:quality(T).

