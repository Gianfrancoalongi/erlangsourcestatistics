-module(calibrator).
-export([run/1]).
-include("ess.hrl").

run(File) ->
    Tree = ess:file(File, [], []),
    R = recalculate_quality(Tree),
    io:format("~p~n",[Tree]),
    io:format("~p ==> ~p~n",[Tree#tree.quality_penalty, R]).

recalculate_quality(T=#tree{}) ->
    ess:quality(T).

