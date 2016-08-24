-module(ess_graphics).
-include("ess.hrl").
-compile(export_all).

-record(node,{id, name, quality, color, children_ids, collapsed=false, render=false,
              quality_penalty}).
-record(edge,{id, to}).

analyse(Path) ->
    SGC = ess:dir(Path),
    SGC2 = ess:quality(SGC),
    Level = 100,
    SGC3 = prune_tree_on_quality(SGC2, Level),
    SGC4 = set_node_ids(SGC3),
    SGC5 = mark_collapsed_nodes(SGC4),
    SGC6 = mark_first_render(SGC5),
    RawNDS = lists:flatten(generate_node_data_set(SGC6)),
    RawEDS = lists:flatten(generate_edges_data_set(SGC6)),
    {VisibleNDS, HiddenNDS} = lists:partition(fun(N) -> N#node.render end, RawNDS),
    io:format("# nodes: ~p~n", [new_unique_id()]),
    VNDS = to_node_string(VisibleNDS),
    HNDS = to_node_string(HiddenNDS),
    VEDS = to_edge_string(RawEDS),
    generate_html_page(VNDS, VEDS, HNDS).
    
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
    file:write_file("/tmp/res.html", list_to_binary(S)).

prune_tree_on_quality(T = #tree{children = Ch}, Level) -> 
    Ch2 = [ C || C <- Ch, C#tree.quality < Level ],
    Ch3 = [ prune_tree_on_quality(C, Level) || C <- Ch2 ],
    T#tree{children = Ch3}.

set_node_ids(T = #tree{children = Ch}) ->
    Id = new_unique_id(),
    T#tree{id=Id, 
           children=set_node_ids_children(Ch)
          }.

set_node_ids_children(Ch) ->
    ChWithId = [ C#tree{id = new_unique_id()} || C <- Ch ],
    [ C#tree{children=set_node_ids_children(C#tree.children)} || C <- ChWithId ].

generate_node_data_set(T=#tree{children=Ch}) ->
    S = generate_one_node(T),
    ChDataSet = [ generate_node_data_set(C) || C <- Ch],
    [ S | ChDataSet].

generate_edges_data_set(#tree{children=[]}) ->
    [];
generate_edges_data_set(#tree{collapsed=true}) ->
    [];
generate_edges_data_set(#tree{id=Id, children=Ch}) ->
    ChIds = [ C#tree.id || C <- Ch, C#tree.quality < 100 ],
    Edges = [ generate_one_edge(Id, ChId) || ChId <- ChIds ],
    ChEdges = [ generate_edges_data_set(C) || C <- Ch],
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

new_unique_id() ->
    Old = case get(unique_id) of
              X when is_integer(X) -> X;
              _ -> 0
          end,
    New = Old+1,
    put(unique_id, New),
    New.

adjust_paths() ->
    add_path("/local/scratch/etxpell/proj/sgc/sgc/ecop/out/").

add_path(Path) ->
    code:add_pathz(Path).

i2l(X) when is_list(X) -> X;
i2l(X) when is_integer(X) -> integer_to_list(X).
