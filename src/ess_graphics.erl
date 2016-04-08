-module(ess_graphics).
-include("ess.hrl").
-export([generate/1]).
-compile(export_all).

-export([t/0, gen_res/0]).
-define(RESULT_DIR, "/home/etxpell/dev_patches/ESS-pages/").

gen_res() ->
    RootDir = "/local/scratch/etxpell/proj/sgc",
    adjust_paths(RootDir),
    
    SGC = do_tree(sgc_dirs(RootDir) ++ syf_dirs(RootDir)),
    file:write_file("./res.data", term_to_binary(SGC)).
    
t() ->
    T = get_tree(),
    generate_all(T),
    ok.

do_tree(Dirs) ->
    RawChildren = [ ess:dir(P, [], "sbg_inc.conf") || P <- Dirs ],
    Children = remove_empty_trees(RawChildren),
    #tree{name="SGC-top",
          value = ess:aggregate_trees(Children),
          children = Children}.

get_tree() ->
    {ok,Bin}  = file:read_file("./res.data"),
    binary_to_term(Bin).

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
    EcopDir = filename:join(Root, "src/syf/ecop/out"),
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
    file:write_file(FileName, HTML).

generate_js_charts(Categories, DivIds, Data) ->
    Z = lists:zip(Categories, DivIds),
    lists:map(
      fun({Tag,DivId}) ->
              TagData = gv(Tag, Data),
              RawData = [ {get_good_name(Dir), Val} || {Dir, Val} <- TagData ],
              DataPoints = generate_datapoints(RawData),
              MaxY = 5+maximum_average(RawData),
              Header = capitalize(a2l(Tag)),
              generate_chart_js(DivId, Header, MaxY, DataPoints)
      end,
     Z).

%% we need to prepare the data so that we can have generic graph generation fuanctions
%% that consume some kind of nice data format 
%% {arity, [{oab,#value{}},{reg,#value{}}...]
%% The generate_chart function should be recursive
tag_transpose(#tree{name=N, value=Value, children=[]}) ->
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
    E = {C#tree.name, gv(Tag, C#tree.value)},
    [E|tag_values(Tag, R)].


get_analyis_categories(L) when is_list(L) ->
    get_analyis_categories(hd(L));
get_analyis_categories(#tree{value = Values})  ->
    [T || {T, _} <- Values ].

maximum_average(RawData) ->
    lists:max([ avg_value(Value) || {_,Value} <- RawData ]).

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

generate_chart_js(DivId, Header, MaxY, DataPoints) ->
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
        valueFormatString:\"#.\",
        maximum: "++i2l(round(1.1*MaxY))++",
        interval: "++i2l(round(MaxY/10)+1)++"
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
