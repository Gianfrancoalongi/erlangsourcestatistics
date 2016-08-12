-module(calibrator).
-export([run/1]).
-include("ess.hrl").

run(File) ->
    Tree = ess:file(File, [], []),
    recalculate_quality(Tree).

recalculate_quality(T=#tree{name = Name,
                            value=Values, 
                            children=Children}) ->
    T#tree{quality = ess:quality(Name, Values),
           children = [ recalculate_quality(C) || C <- Children ]
          }.

