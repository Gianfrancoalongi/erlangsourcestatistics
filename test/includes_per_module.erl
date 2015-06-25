-module(includes_per_module).
-compile({parse_transform,ess}).
-include_lib("eunit/include/eunit.hrl").
-include("test/something.hrl").
-export([a/0]).
a() ->
    ?this.
