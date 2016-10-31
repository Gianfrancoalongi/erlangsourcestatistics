-module(ess).

-include("ess.hrl").

-export([analyse/1,
         analyse/2]).
-export([get_options/2,
         get_default_options/0
        ]).

analyse(Path) ->
    analyse(Path, []).
analyse(Path, CommandLineOpts) ->
    T1 = erlang:monotonic_time(),
    Opts = get_options(Path, CommandLineOpts),
    seq(Path, Opts,
        [ fun init_timing/1,
          fun ess_engine:dir/2,
          fun ess_engine:quality/2,
          fun set_top_level_name/1,
          fun save_csv_file/2,
          fun ess_graphics:generate/2,
          fun print_timing/1
        ]).

get_options(Dir, CommandLineOpts) ->
    find_all_opts([{target_dir, Dir} | CommandLineOpts]).

find_all_opts(LineOpts) ->
    FileOpts = find_file_opts(LineOpts),
    DefaultOptions = get_default_options(),
    Unique = lists:ukeysort(1, LineOpts ++ FileOpts ++ DefaultOptions),
    set_metric_penalty(Unique).

set_metric_penalty(Opts) ->
    Ms = gv(metrics, Opts),
    Defaults = default_metrics(),
    WithPenalties = [ set_each_metric_penalty(M, Defaults) || M <- Ms ],
    lists:keyreplace(metrics, 1, Opts, {metrics, WithPenalties}).

set_each_metric_penalty(M, Defaults) when is_atom(M) ->
    proplists:lookup(M, Defaults);
set_each_metric_penalty(M, _Defaults) ->
    M.

get_default_options() ->
    [{metrics, default_metrics()},
     {include_paths, []},
     {conf_dir, ""},
     {exclude_dir_patterns, [".git"]},
     {exclude_dir_patterns_during_hrl_search, []},
     {exclude_dir_patterns_during_analysis, []},
     {parse_transform_beam_dirs, []},
     {out_dir, "./"}].

default_metrics() ->
    [{function_naming, {0, 1}},
     {variable_naming, {1, 5}},
     {arity, {3, 8}},
     {clauses, {4, 10}},
     {expressions_per_function, {20, 50}},
     {variable_steppings, {1, 5}},
     {export_all, {0,1}},
     {space_after_comma, {3, 10}},
     {warnings, {0, 1}},
     {nested_clauses, {1, 3}},

     {complexity, {2, 6}},
     {line_lengths, {75, 120}}].

find_file_opts(LineOpts) ->
    HomeDir = get_home_dir(),
    LineOptionDir = gv(conf_dir, LineOpts),
    TargetDir = gv(target_dir, LineOpts),
    {Path, Opts} = try_paths_in_order([LineOptionDir,
                                       TargetDir,
                                       ".",
                                       HomeDir]),
    io:format("config file ~p~n", [Path]),
    adjust_all_relative_paths(Path, Opts).

adjust_all_relative_paths(Path, Opts) ->
    Keys = [include_paths, parse_transform_beam_dirs],
    Present = [ K || K <- Keys, gv(K, Opts) =/= undefined ],
    adjust_relative_paths(Path, Present, Opts).

adjust_relative_paths(RootPath, [K | Keys], Opts) ->
    RelPaths = gv(K, Opts),
    P2 = adjust_relative_paths(RootPath, RelPaths),
    [{K, P2} | adjust_relative_paths(RootPath, Keys, Opts)];
adjust_relative_paths(_RootPath, [], Opts) ->
    Opts.

adjust_relative_paths(RootPath, Paths) ->
    Fun = fun(P) ->
                  case filename:pathtype(P) of
                      relative -> filename:join(RootPath, P);
                      _ -> P
                  end
          end,
    lists:map(Fun, Paths).


get_home_dir() ->
    User = os:getenv("USER"),
    filename:join(["/home",User]).

try_paths_in_order([]) ->
    [];
try_paths_in_order([undefined|T]) ->
    try_paths_in_order(T);
try_paths_in_order([P|T]) ->
    Path = filename:join(P,"ess.conf"),
    case file:consult(Path) of
        {ok, Terms} ->
            {P, Terms};
        _ ->
            try_paths_in_order(T)
    end.

set_top_level_name(T=#tree{name=Name}) ->
    case rev(filename:split(Name)) of
        ["src", "sgc" | _ ] -> T#tree{name="SBG"};
        ["src", "is-sbg" | _] -> T#tree{name="SBG"};
        _ -> T
    end.

save_csv_file(T, Opts) ->
    L = format_tree("", T),
    File = filename:join(gv(out_dir, Opts),"res.csv"),
    file:write_file(File, list_to_binary(L)),
    T.

format_tree(_Pad, #tree{type=function}) ->
    [];
format_tree(Pad, T=#tree{quality=Q, children=Ch}) ->
    [io_lib:format("~s~s, ~p~n", [Pad, tree_name(T), Q]),
     format_tree(Pad++"    ", Ch)];
format_tree(Pad, L) when is_list(L) ->
    [format_tree(Pad, C) || C <- L].

tree_name(#tree{type=file, name=Name}) -> filename:basename(Name);
tree_name(#tree{name=Name}) -> Name.

init_timing(Tree) ->
    put(start_time, erlang:monotonic_time()),
    Tree.

print_timing(Tree) ->
    T1 = get(start_time),
    TDiff = erlang:monotonic_time()-T1,
    UnitPerS = erlang:convert_time_unit(1, seconds, native),
    TDiffMs = round(1000 * TDiff / UnitPerS),
    io:format("evaluating took: ~pms~n", [TDiffMs]),
    Tree.


seq(A1, A2, [F|L]) ->
    A = case erlang:fun_info(F, arity) of
            {arity, 1} -> F(A1);
            {arity, 2} -> F(A1, A2)
        end,
    seq(A, A2, L);
seq(Data, _, []) ->
    Data.

rev(L) -> lists:reverse(L).

gv(Key, L) ->
    proplists:get_value(Key, L).
