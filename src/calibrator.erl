-module(calibrator).
-export([run/1]).
-include("ess.hrl").

run(File) ->
    Tree = ess:file(File, [], []),
    R = recalculate_quality(Tree),
    io:format("~p~n",[R]).

recalculate_quality(T=#tree{name = Name,
                            raw_values=Values}) ->
    ess:quality(Name, Values).

