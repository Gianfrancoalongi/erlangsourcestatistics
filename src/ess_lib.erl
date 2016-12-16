-module(ess_lib).

-export([log/3,
         reset_log/1]).

-export([gv/2,
         gv/3]).

-include("ess.hrl").

log(Type, Fmt, Args) ->
    File = log_file(Type),
    Message = io_lib:format(Fmt, Args),
    file:write_file(File, Message, [append]).

reset_log(Type) ->
    File = log_file(Type),
    file:write_file(File, <<>>).

log_file(error) ->
    "/tmp/ess_errors.log";
log_file(dup) ->
    "/tmp/ess_duplicates.log";
log_file(run) ->
    "/tmp/ess.log".
    
gv(Key, L) ->
    proplists:get_value(Key, L).
gv(Key, L, Def) ->
    proplists:get_value(Key, L, Def).


