-module(ess_graphics).
-include("ess.hrl").
-export([generate/1]).
-compile(export_all).
-define(RESULT_DIR, "/home/etxpell/dev_patches/ESS-pages/").

t() ->
    RootDir = "/local/scratch/etxpell/proj/sgc/src/sgc/reg/src",
    %%RootDir = "/local/scratch/etxpell/proj/erlangsourcestatistics/calibration/",
    adjust_paths(RootDir),
    SGC = ess:dir(RootDir),
    SGC2 = ess:quality(SGC),
    Level = 100,
    SGC3 = prune_tree_on_quality(SGC2, Level),
    SGC4 = set_node_ids(SGC3),
    RawNDS = lists:flatten(generate_node_data_set(SGC4)),
    RawEDS = lists:flatten(generate_edges_data_set(SGC4)),
    NDS = to_node_string(RawNDS),
    EDS = to_edge_string(RawEDS),
    io:format("# nodes: ~p~n", [new_unique_id()]),
    generate_html_page(NDS, EDS).

generate_html_page(NDS, EDS) ->
    S = "<!doctype html>
<html><head> <title>Network | Basic usage</title>
  <script type=\"text/javascript\" src=\"http://visjs.org/dist/vis.js\"></script>
  <style type=\"text/css\">
    #mynetwork {
      width: 1000px;
      height: 600px;
      improvedLayout: false;
      border: 1px solid lightgray;
    }
  </style>
</head>
<body>
<p>
  Create a simple network with some nodes and edges.
</p>
<div id=\"mynetwork\"></div>

<script type=\"text/javascript\">
  // create an array with nodes
  var nodes = new vis.DataSet(["++NDS++"
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
  var options = {layout: {improvedLayout:true},
                 physics:{enabled:true} };
  var network = new vis.Network(container, data, options);
  network.on(\"doubleClick\", function (params) {
           collapse_uncollapse_node(parseInt(params.nodes));
    });

  function collapse_uncollapse_node(Id) {
        nodes.forEach(function(n) {
                        if(n.childof.indexOf(Id) != -1) {
                           n.hidden = ! n.hidden;
                           edges.forEach(function(e){ 
                               if(e.from == Id && e.to == n.id) {
                                  e.hidden = n.hidden;
                                  edges.update(e);
                               }
                           },this);
                           nodes.update(n);                           
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
           children=set_node_ids_children(Ch, [Id])
          }.

set_node_ids_children(Ch, ChildOf) ->
    ChWithId = [ C#tree{id = new_unique_id(),
                        child_of = ChildOf
                       } || C <- Ch ],
    [ begin
          Id = C#tree.id,
          C#tree{children=set_node_ids_children(C#tree.children, [Id|ChildOf])} 
      end || C <- ChWithId ].

prune_graph_on_number_of_nodes(T = #tree{children=[]}, _) ->
    T;
prune_graph_on_number_of_nodes(T = #tree{id=Id, children=Ch}, MaxNodes) ->
    LastChild = last_child(T),
    case shall_we_prune(LastChild, MaxNodes) of
        true ->
            Ch2 = remove_all_children(Ch),
            T#tree{children = Ch2};
        false ->
            case shall_we_prune(last_child(LastChild), MaxNodes) of
                true ->
                    Ch2 = remove_all_children(Ch),
                    T#tree{children = Ch2};
                false ->
                    Ch2 = [ prune_graph_on_number_of_nodes(C, MaxNodes) || C <- Ch ],
                    T#tree{children = Ch2}
            end
    end.

shall_we_prune(T, MaxNodes) ->
    T#tree.id > MaxNodes.

last_child(#tree{children=Ch}) ->
    lists:last(Ch).

remove_all_children(Ch) ->
    [ C#tree{children = []} || C <- Ch ].

generate_node_data_set(T=#tree{children=Ch}) ->
    S = generate_one_node(T),
    ChDataSet = [ generate_node_data_set(C) || C <- Ch],
    [ S | ChDataSet].

generate_edges_data_set(#tree{children=[]}) ->
    [];
generate_edges_data_set(T=#tree{id=Id, children=Ch}) ->
    ChIds = [ C#tree.id || C <- Ch, C#tree.quality < 100 ],
    Edges = [ genereate_one_edge(Id, ChId) || ChId <- ChIds ],
    ChEdges = [ generate_edges_data_set(C) || C <- Ch],
    Edges ++ ChEdges .

generate_one_node(#tree{id=Id, name=Name, quality=Q, child_of=CO}) ->
    Color = quality_to_color(Q),
    {Id, filename:basename(Name), Q, Color, CO}.

quality_to_color(N) ->
    NN = max(N, 0),
    G = round(255*NN/100),
    R = 255-G,
    B = 90,
    {R, G, B}.

to_node_string(L) ->
    S = [nice_str("{id: ~p, label: \"~s\\n~p\", color: '~s', childof: ~p}", 
                  [Id, 
                   Name, 
                   round(Quality), 
                   rgba(Color),
                   CO])
         || {Id, Name, Quality, Color, CO} <- L],
    string:join(S, ",\n").

to_edge_string(L) ->
    S = [nice_str("{from: ~p, to: ~p, color:'black'}", [Id, Name]) || {Id, Name} <- L],
    string:join(S, ",").
    
rgba({R,G,B}) ->
    "rgba("++i2l(R)++","++i2l(G)++","++i2l(B)++",1)".

nice_str(F,A) ->
    lists:flatten(io_lib:format(F, A)).

genereate_one_edge(Id, ChId) ->
    {Id, ChId}.

new_unique_id() ->
    Old = case get(unique_id) of
              X when is_integer(X) -> X;
              _ -> 0
          end,
    New = Old+1,
    put(unique_id, New),
    New.




sgc_dirs(RootDir) ->
    block_dirs(filename:join(RootDir, "src/sgc"), sgc_blocks()).

syf_dirs(RootDir) ->
    block_dirs(filename:join(RootDir, "src/syf"), syf_blocks()).

block_dirs(Dir, Blocks) ->
    [ filename:join([Dir, D, "src"]) || D <- Blocks ].

sgc_blocks() -> 
    [ b2b, cha, dia, hiw, mph, oab, reg, sgm, sgn, sni, tra, trc ].

syf_blocks() -> 
    [ blm, ccpc, comte, cpl, ecop, esc,
      gcp, generic, hcfa, om, omgen, oms, perf, plc, 
      pmf, pms, rcm, sbm, "sctp/sctp_erl", sip, smm, swm, "sys/sys_erl" ].
        
adjust_paths(Root) ->
    EcopDir = filename:join(Root, "syf/ecop/out"),
    add_path(EcopDir).

add_path(Path) ->
    code:add_pathz(Path).



generate_all(#tree{children=[]}) ->
    [];
generate_all(Tree=#tree{children=Children}) ->
    generate(Tree),
    [ generate_all(C) || C <- Children ].

generate(Tree) ->
    {Name, Data} = tag_transpose(Tree),
    Categories = get_analyis_categories(Tree),
    DivIds = lists:seq(1,length(Categories)),
    generate_chart_page(Name, Categories, DivIds, Data).

generate_chart_page(Name, Categories, DivIds, Data) ->
    DstDir = ?RESULT_DIR,
    filelib:ensure_dir(DstDir),
    FileName = filename:join(DstDir, Name++"_analysis")++".html",
    JS = generate_js_charts(Categories, DivIds, Data),
    Divs = generate_divs(DivIds),
    Table = generate_table(Divs),
    HTML = generate_html(Table, JS),
    case file:write_file(FileName, HTML) of
        ok ->
            ok;
        Err ->
            io:format("Error writing page ~p : ~p~n",[FileName, Err])
    end.

generate_js_charts(Categories, DivIds, Data) ->
    Z = lists:zip(Categories, DivIds),
    lists:map(
      fun({Tag,DivId}) ->
              TagData = gv(Tag, Data),
              RawData = [ {get_good_name(Dir), Val} || {Dir, Val} <- TagData ],
              DataPoints = generate_datapoints(RawData),
              Header = capitalize(a2l(Tag)),
              generate_chart_js(DivId, Header, DataPoints)
      end,
     Z).

%% we need to prepare the data so that we can have generic graph generation fuanctions
%% that consume some kind of nice data format 
%% {arity, [{oab,#value{}},{reg,#value{}}...]
%% The generate_chart function should be recursive
tag_transpose(#tree{name=N, quality_penalty=Value, children=[]}) ->
    {get_good_name(N), Value};
tag_transpose(#tree{name=N, children=Children}) ->
    {get_good_name(N), tag_transpose_children(Children)}.

tag_transpose_children(Children) ->
    Tags = get_analyis_categories(Children),
    [ {Tag, tag_values(Tag, Children)}  || Tag <- Tags ].

remove_empty_trees(L) ->
    [ T || T <- L, is_record(T, tree) ].

tag_values(_, []) -> 
    [];
tag_values(Tag, [C|R]) ->
    E = {C#tree.name, gv(Tag, C#tree.quality_penalty)},
    [E|tag_values(Tag, R)].


get_analyis_categories(L) when is_list(L) ->
    get_analyis_categories(hd(L));
get_analyis_categories(#tree{quality_penalty = Values})  ->
    [ T || {T, _} <- Values ].

maximum_average(RawData) ->
    lists:max([ avg_value(Value) || {_,Value} <- RawData ]).

minimum(RawData) ->
    lists:min([ avg_value(Value) || {_,Value} <- RawData ]).

avg_value(#val{avg = Value}) ->
    Value;
avg_value(Value) when is_integer(Value) ->
    Value;
avg_value(_) ->
    0.


generate_datapoints(RawData) ->
    lists:map(fun generate_datapoint/1, RawData).

generate_datapoint({Block,#val{max=Max, avg=Avg}}) ->
    io_lib:format("{ y: ~p, z: ~p, label:\"~s\"}", [Avg, Max, Block]);
generate_datapoint({Label, Value}) ->
    io_lib:format("{ y: ~p, z: ~p, label:\"~s\"}", [Value, Value, Label]).

get_good_name(#tree{name=Dir}) ->
    get_good_name(Dir);
get_good_name(Dir) ->
    case lists:reverse(filename:split(Dir)) of
        ["src",  Block | _] -> Block;
        [FileName, "src" | _] -> filename:rootname(FileName);
        [Name |_] -> Name
    end.

gv(K,L) ->
    proplists:get_value(K,L).

a2l(X) when is_list(X) -> X;
a2l(X) when is_atom(X) -> atom_to_list(X).

i2l(X) when is_list(X) -> X;
i2l(X) when is_integer(X) -> integer_to_list(X).

capitalize([C|R]) when (C>=$a) , (C=<$z) ->
    [C-32 | R];
capitalize(L) ->
    L.

generate_divs(DivIds) ->
    [ "<div id=\"chartContainer"++i2l(Id)++"\" style=\"height: 300px; width: 100%;\"></div>"
      || Id <- DivIds].

generate_table(Divs) ->
    "<table style=\"width:100%\">"++table_with_2_elements_per_row(Divs)++
        "</table>".

table_with_2_elements_per_row([]) -> "";
table_with_2_elements_per_row([E1,E2|R]) ->
    "<tr>
      <td>"++E1++"</td>
      <td>"++E2++"</td>
    </tr>"++
    table_with_2_elements_per_row(R);
table_with_2_elements_per_row([E1]) ->
    "<tr>
      <td>"++E1++"</td>
      <td></td>
    </tr>".

generate_html(Table,JSs) ->
"<!DOCTYPE HTML>
<html>

<head>  
  <script type=\"text/javascript\">
  window.onload = function () {"
++string:join(JSs,"\n")++"
}
</script>
<script type=\"text/javascript\" src=\"http://canvasjs.com/assets/script/canvasjs.min.js\"></script>
</head>
<body>"++Table++
"</body>
</html>".

generate_chart_js(DivId, Header, DataPoints) ->
  "var chart_"++Header++" = new CanvasJS.Chart(\"chartContainer"++i2l(DivId)++"\",
    {
      zoomEnabled: true,
      animationEnabled: true,
      title:{
        text: \""++Header++"\"
      },
      axisX: {
        title:\"Block\",
        labelAngle: -30,
        interval: 1
      },
      axisY:{
        title: \""++Header++"\",
        gridThickness: 1,
        tickThickness: 1,
        gridColor: \"lightgrey\",
        tickColor: \"lightgrey\",
        lineThickness: 0,
        valueFormatString:\"#.\"
      },

      data: [
      {        
        type: \"bubble\",     
        toolTipContent: \"<span style='\\\"'color: {color};'\\\"'><strong>{label}</strong></span><br/> <strong>Max "++Header++"</strong> {z} <br/> <strong>Mean "++Header++"</strong> {y} <br/>\",

        click: function(e) { window.location.href = e.dataPoint.label+\"_analysis.html\"; },

        dataPoints: 
        ["
++string:join(DataPoints,",\n")++" 
        ]
       }
      ]
    });
   chart_"++Header++".render();
".
