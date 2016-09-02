-module(ess_graphics).
-include("ess.hrl").
-compile(export_all).

-record(node,{id, name, quality, color, children_ids, collapsed=false, render=false,
              quality_penalty}).
-record(edge,{id, to}).

%% before opt
%%
%% evaluating took: 113100ms
%%
%%
%% after parallell opt
%%
%% evaluating took: 41090ms
%%

analyse(Path) ->
    T1 = erlang:monotonic_time(),
    Tree =
        seq(Path,
            [ fun ess:dir/1,
              fun ess:quality/1,
              fun print_tree/1,
              fun prune_nodes_with_single_children/1,
              fun prune_tree_on_quality/1,
              fun set_tree_ids/1,
              fun mark_collapsed_nodes/1,
              fun mark_first_render/1]),
    TDiff = erlang:monotonic_time()-T1,
    UnitPerS = erlang:convert_time_unit(1, seconds, native),
    TDiffMs = round(1000 * TDiff / UnitPerS),
    io:format("evaluating took: ~pms~n", [TDiffMs]),
    generate_html_page(Tree).

print_tree(T=#tree{name=Name, children=Ch}) ->
    io:format("tree ~p~n", [T#tree.name]),
    [io:format("        ~p~n", [C#tree.name]) || C <- Ch],
    T.


generate_html_page(Tree) ->
    RawNDS = generate_node_data_set(Tree),
    RawEDS = generate_edges_data_set(Tree),
    {VisibleNDS, HiddenNDS} = split_visible(RawNDS),
    io:format("# nodes: ~p~n", [new_unique_id()]),
    VNDS = to_node_string(VisibleNDS),
    HNDS = to_node_string(HiddenNDS),
    VEDS = to_edge_string(RawEDS),
    generate_html_page(VNDS, VEDS, HNDS).

split_visible(NDS) ->
    lists:partition(fun(N) -> N#node.render end, NDS).


t() ->
    RootDir = "/local/scratch/etxpell/proj/sgc/src/",
    adjust_paths(),
    analyse(RootDir).

mark_collapsed_nodes(L) when is_list(L) ->
    [mark_collapsed_nodes(T) || T <- L];
mark_collapsed_nodes(T=#tree{children=Ch}) when length(Ch) == 1 ->
    T#tree{children=mark_collapsed_nodes(Ch)};
mark_collapsed_nodes(T=#tree{children=Ch}) ->
    T#tree{children=set_collapsed(Ch)}.

set_collapsed(L) ->
    [T#tree{collapsed=true} || T <- L].
    

mark_first_render(L) when is_list(L) ->
    [mark_first_render(T) || T <- L];
mark_first_render(T=#tree{collapsed=true}) ->
    T#tree{render=true};
mark_first_render(T=#tree{children=Ch}) ->
    T#tree{render=true, children=mark_first_render(Ch)}.

generate_html_page(NDS, EDS, HIDDEN_NDS) ->
    S = "<!doctype html>
<html><head> <title>Software Quality Graph</title>
  <script type=\"text/javascript\" src=\"http://visjs.org/dist/vis.js\"></script>
  <style type=\"text/css\">
    #mynetwork {
      width: 100vw;
      height: 100vh;
      border: 1px solid lightgray;
    }

    div.vis-network-tooltip {
      position: absolute;
      visibility: hidden;
      padding: 5px;
      white-space: nowrap;

      font-family: verdana;
      font-size:14px;
      color:#000000;
      background-color: #f5f4ed;

      -moz-border-radius: 3px;
      -webkit-border-radius: 3px;
      border-radius: 3px;
      border: 1px solid #808074;

      box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
      pointer-events: none;
    }

  </style>
</head>
<body>
<div id=\"mynetwork\" height=100%, width=100%></div>

<script type=\"text/javascript\">
  // create an array with nodes
  var nodes = new vis.DataSet(["++NDS++"
  ]);

  // create an array with ALL nodes
  var all_nodes = new vis.DataSet(["++HIDDEN_NDS++","++NDS++"
  ]);

  // create an array with edges
  var edges = new vis.DataSet(["++EDS++"
  ]);

  // create a network
  var container = document.getElementById('mynetwork');
  var data = {
    nodes: nodes,
    edges: edges
  };
  var options = {layout:{improvedLayout:true},
                 physics:{enabled:true,
                          repulsion:{nodeDistance:300}},
                 interaction:{hover:true},

                 nodes: {
                   shape: 'box',
                   font: { size: 10, color: 'black'},
                   borderWidth: 2
                 },

                 edges: {width: 1}

                 };
  var network = new vis.Network(container, data, options);

  network.on(\"doubleClick\", function (params) {
           collapse_uncollapse_node(parseInt(params.nodes));
    });

  function collapse_uncollapse_node(Id) {
       all_nodes.forEach(function(n) {
                        if(n.id == Id) {
                           if( n.collapsed ) {
                              n.children_ids.forEach(function(ChId) {
                                  add_node_with_id(ChId);
                                  add_edge_from_to(Id,ChId);
                                  n.collapsed=false;
                                  all_nodes.update(n);
                              },this);
                           }else{
                              n.children_ids.forEach(function(ChId) {
                                  remove_node_with_id(ChId);
                                  remove_edge_from(Id);
                                  n.collapsed=true;
                                  all_nodes.update(n);
                              },this);
                           }
                        }
                      },this);
    }

  function add_node_with_id(Id) {
        all_nodes.forEach(function(n) {
                        if(n.id == Id) { 
                           nodes.add(n);
                        }
                      },this);
  }

  function add_edge_from_to(Id, ChId) {
        edges.add({from: Id, to: ChId, color: 'black'});
  }

  function remove_node_with_id(Id) {
       nodes.forEach(function(n) {
                       if(n.id == Id) { 
                          nodes.remove(n);
                       }
                     },this);
  }

  function remove_edge_from(Id) {
      edges.forEach(function(e) {
                      if(e.from == Id ) {
                          edges.remove(e);
                      }
                   },this);
  }

</script>
</body>
</html>",
    file:write_file("res.html", list_to_binary(S)).

prune_nodes_with_single_children(T=#tree{type=dir, children=[OneCh]}) ->
    NewCh = prune_nodes_with_single_children(OneCh#tree.children),
    io:format("pruning ~p to ~p~n", [OneCh#tree.name, T#tree.name]),
    T#tree{children=NewCh};
prune_nodes_with_single_children(T=#tree{children=Ch}) ->
    T#tree{children=prune_nodes_with_single_children(Ch)};
prune_nodes_with_single_children(L) when is_list(L) ->
    [prune_nodes_with_single_children(T) || T <- L].

    
prune_tree_on_quality(T) ->
    prune_tree_on_quality(T, 100).
        
prune_tree_on_quality(T = #tree{children = Ch}, Level) -> 
    Ch2 = [ C || C <- Ch, C#tree.quality < Level ],
    Ch3 = [ prune_tree_on_quality(C, Level) || C <- Ch2 ],
    T#tree{children = Ch3}.

set_tree_ids(T) ->
    reset_unique_id(),
    set_tree_id_width_first(set_tree_id(T)).

set_tree_id_width_first(T) ->
    OldId = read_unique_id(),
    T2 = set_id_of_one_level(T),
    %% Keep recursing until no ids have been set
    case read_unique_id() of
        OldId -> T;
        _ -> set_tree_id_width_first(T2)
    end.

set_id_of_one_level(T=#tree{children=[]}) ->
    T;
set_id_of_one_level(L) when is_list(L) ->
    [set_id_of_one_level(T) || T <- L];
set_id_of_one_level(T=#tree{children=Ch}) ->
    case children_has_ids_already(Ch) of
        true -> T#tree{children=set_id_of_one_level(Ch)};
        _ -> T#tree{children=set_tree_id(Ch)}
    end.

children_has_ids_already([#tree{id=Id}|_]) when is_integer(Id) -> true;
children_has_ids_already(_) -> false.

set_tree_id(L) when is_list(L) ->
    [set_tree_id(T) || T <- L];
set_tree_id(T=#tree{}) ->
    T#tree{id=new_unique_id()}.


generate_node_data_set(T) ->
    lists:flatten(generate_node_data_set2(T)).
generate_node_data_set2(T=#tree{children=Ch}) ->
    S = generate_one_node(T),
    [S | [generate_node_data_set2(C) || C <- Ch]].


generate_edges_data_set(T) ->
    lists:flatten(generate_edges_data_set2(T)).

generate_edges_data_set2(#tree{children=[]}) ->
    [];
generate_edges_data_set2(#tree{collapsed=true}) ->
    [];
generate_edges_data_set2(#tree{id=Id, children=Ch}) ->
    ChIds = [ C#tree.id || C <- Ch, C#tree.quality < 100 ],
    Edges = [ generate_one_edge(Id, ChId) || ChId <- ChIds ],
    ChEdges = [ generate_edges_data_set2(C) || C <- Ch],
    Edges ++ ChEdges.

generate_one_node(#tree{id=Id, name=Name, quality=Q, 
                        children=Ch, render=Render, collapsed=Collapsed,
                        quality_penalty=QP
                       }) ->
    Color = quality_to_color(Q),
    #node{id=Id, 
          name=replace_pipe_with_slash(filename:basename(Name)),
          quality=Q, 
          color=Color, 
          children_ids=[C#tree.id||C<-Ch],
          render=Render,
          collapsed=Collapsed,
          quality_penalty=QP
         }.

replace_pipe_with_slash([]) ->
    [];
replace_pipe_with_slash([$||T]) ->
    [$/|replace_pipe_with_slash(T)];
replace_pipe_with_slash([C|T]) ->
    [C|replace_pipe_with_slash(T)].

generate_one_edge(Id, ChId) ->
    #edge{id=Id, 
          to=ChId}.

quality_to_color(N) ->
    NN = max(N, 0),
    G = round(255*NN/100),
    R = 255-G,
    B = 90,
    {R, G, B}.

to_node_string(L) ->
    S = [ nice_str("{id: ~p, label: \"~s\\n~p\", color: '~s', children_ids: ~w, "
                    "collapsed:~p, title:\"~s\", mass:~p, font:{size:~p, color:'black'}}",
                  [N#node.id,
                   N#node.name,
                   round(N#node.quality),
                   rgba(N#node.color),
                   N#node.children_ids,
                   N#node.collapsed,
                   quality_penalty_to_title(N#node.quality_penalty),
                   quality_to_mass(N#node.quality),
                   quality_to_font_size(N#node.quality)
                  ])
         || N <- L],
    string:join(S, ",\n").

quality_to_mass(Q) ->
    round(10 - (10 / abs(100-Q))).

quality_to_font_size(Q) ->
    round(40 - (40 / abs(100-Q))).

quality_penalty_to_title(QP) ->
    string:join([ lists:flatten(io_lib:format("~p: ~p",[K,V])) || {K,V}<-QP],"</br> ").

to_edge_string(L) ->
    S = [nice_str("{from: ~p, to: ~p, color:'black'}", 
                  [E#edge.id, 
                   E#edge.to
                  ]) || E <- L],
    string:join(S, ",").
    
rgba({R,G,B}) ->
    "rgba("++i2l(R)++","++i2l(G)++","++i2l(B)++",1)".

nice_str(F,A) ->
    lists:flatten(io_lib:format(F, A)).

reset_unique_id() ->
    erase(unique_id).

read_unique_id() ->
    case get(unique_id) of
        X when is_integer(X) -> X;
        _ -> 0
    end.

new_unique_id() ->
    New = read_unique_id()+1,
    put(unique_id, New),
    New.

adjust_paths() ->
    add_path("/local/scratch/etxpell/proj/sgc/sgc/ecop/out/").

add_path(Path) ->
    code:add_pathz(Path).

i2l(X) when is_list(X) -> X;
i2l(X) when is_integer(X) -> integer_to_list(X).

seq(Data, [F|L]) -> seq(F(Data), L);
seq(Data, []) -> Data.

rev(L) -> lists:reverse(L).
