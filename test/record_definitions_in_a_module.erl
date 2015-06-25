-module(record_definitions_in_a_module).
-compile({parse_transform,ess}).
-export([something/0,
         else/1,
         entirely/0]).

-record(abba,{music,festival}).

something() ->
    something_is_ongoing.

-record(baab,{bananas}).

else(If) ->
    case If of
        'if' ->
            if If ->
                    ok;
               _ ->
                    true
            end;
        _ ->
            false
    end.

-record(world_record,{breaking,
                      news}).

entirely(0) ->
    1;
entirely(1) ->
    2.

-record(this,{that,
              them}).
              
