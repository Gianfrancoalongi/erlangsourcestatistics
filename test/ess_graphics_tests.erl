-module(ess_graphics_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/ess.hrl").
-compile(export_all).


set_tree_ids_one_test() ->
    T = #tree{},
    Expected = #tree{id=1},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

set_tree_ids_two_test() ->
    T = #tree{children=[#tree{}]},
    Expected = #tree{id=1, children=[#tree{id=2}]},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

set_tree_ids_three_test() ->
    T = #tree{children=[#tree{}, #tree{}]},
    Expected = #tree{id=1, children=[#tree{id=2}, #tree{id=3}]},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

set_tree_ids_four_test() ->
    T = #tree{children=[#tree{children=[#tree{}]}, #tree{}]},
    Expected = #tree{id=1, 
                     children=[#tree{id=2, children=[#tree{id=4}]}, 
                               #tree{id=3}]},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

set_tree_ids_four_2_test() ->
    T = #tree{children=[#tree{}, #tree{children=[#tree{}]}]},
    Expected = #tree{id=1, 
                     children=[#tree{id=2}, 
                               #tree{id=3, children=[#tree{id=4}]}]},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

set_tree_ids_six_test() ->
    T = #tree{children=[#tree{children=[#tree{},#tree{}]}, 
                        #tree{children=[#tree{}]}]},
    Expected = #tree{id=1, 
                     children=[#tree{id=2, children=[#tree{id=4}, 
                                                     #tree{id=5}]}, 
                               #tree{id=3, children=[#tree{id=6}]}]},
    Res = ess_graphics:set_tree_ids(T),
    ?assertEqual(Expected, Res).

