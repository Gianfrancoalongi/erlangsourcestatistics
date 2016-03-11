-module(ess_graphics).
-include("ess.hrl").
-export([generate/1]).

-export([t/0, gen_res/0]).
-define(RESULT_DIR, "/home/etxpell/dev_patches/ESS-pages/").

gen_res() ->
    RootDir = "/local/scratch/etxpell/proj/sgc",
    adjust_paths(RootDir),
    SgcPaths = [ filename:join([RootDir, "src/sgc", D, "src"]) || D <- sgc_dirs() ],
    SyfPaths = [ filename:join([RootDir, "src/syf", D, "src"]) || D <- syf_dirs() ],
    Res = [ ess:dir(P, [], "sbg_inc.conf") || P <- SgcPaths ++ SyfPaths ],
    file:write_file("./res.data", term_to_binary(Res)).


sgc_dirs() -> 
    [ b2b, cha, dia, hiw, mph, oab, reg, sgm, sgn, sni, tra, trc ].

syf_dirs() -> 
    [ blm, ccpc, chstb, chtr, comte, cpl, ecop, esc,
      gcp, generic, hcfa, om, omgen, oms, omstb, perf, plc, 
      pmf, pms, rcm, sbm, "sctp/sctp_erl", sip, smm, swm, "sys/sys_erl" ].
        
adjust_paths(Root) ->
    EcopDir = filename:join(Root, "src/syf/ecop/out"),
    add_path(EcopDir).

add_path(Path) ->
    code:add_pathz(Path).

t() ->
    {ok,Bin}  = file:read_file("./res.data"),
    Res = binary_to_term(Bin),
    generate(Res).

generate(AnalysisResults) ->
    Categories = get_analyis_categories(AnalysisResults),
    DivIds = lists:seq(1,length(Categories)),
    generate_block_charts(Categories, DivIds, AnalysisResults),
    generate_module_charts(Categories, DivIds, AnalysisResults).

generate_block_charts(Categories, DivIds, AnalysisResults) ->
    DstDir = ?RESULT_DIR,
    FileName = filename:join(DstDir, "analysis")++".html",
    generate_chart(FileName, Categories, DivIds, AnalysisResults).

generate_module_charts(_,_,[]) -> ok;
generate_module_charts(Categories, DivIds, [[]|R]) ->
    generate_module_charts(Categories, DivIds, R);
generate_module_charts(Categories, DivIds, [AnalysisResult|R]) ->
    Data = AnalysisResult#tree.children,
    Name = get_good_name(AnalysisResult#tree.name),
    DstDir = ?RESULT_DIR,
    FileName = filename:join(DstDir, Name++"_analysis")++".html",
    generate_chart(FileName, Categories, DivIds, Data),
    generate_module_charts(Categories, DivIds, R).


-- we need to prepare the data so that we can have generic graph generation functions
-- that consume some kind of nice data format 
-- {arity, [{oab,#value{}},{reg,#value{}}...]
-- The generate_chart function should be recursive
-- 

generate_chart(Parent, []) ->
    ok;
generate_chart(Parent, [T|R]) ->
    Categories = get_analyis_categories(T),
    DivIds = lists:seq(1,length(Categories)),
    Name = get_good_name(T#tree.name),
    Values = T#tree.value,
    FileName = filename:join(DstDir, Name++"_analysis")++".html",
    generate_chart(FileName, Categories, DivIds, Values),
    Children = T#tree.children,    
    generate_chart(Name, Children),
    generate_chart(Parent, R).

   
generate_chart(FileName, Categories, DivIds, DataSet) ->
    JSCharts = generate_js_charts(Categories, DivIds, DataSet),
    Divs = generate_divs(DivIds),
    Table = generate_table(Divs),
    HTML = generate_html(Table, JSCharts),
    file:write_file(FileName, HTML).

generate_js_charts(Categories, DivIds, DataSet) ->
    Z = lists:zip(Categories, DivIds),
    lists:map(
      fun({Tag,DivId}) ->
              RawData = [ {get_good_name(Dir), gv(Tag, Value)} ||  
                            #tree{name = Dir, value = Value} <- DataSet ],
              DataPoints = generate_datapoints(RawData),
              case maximum_average(RawData) of 
                  undefined ->
                      io:format("RawData: ~p~n",[RawData]);
                  _ ->
                      ok
              end,
              io:format("Maximum average:~p~n",[maximum_average(RawData)]),
              MaxY = 5+maximum_average(RawData),
              
              Header = capitalize(a2l(Tag)),
              generate_chart_js(DivId, Header, MaxY, DataPoints)
      end,
     Z).

get_analyis_categories(L) when is_list(L) ->
    get_analyis_categories(hd(L));
get_analyis_categories(AnalysisResult)  ->
    [T || {T, _} <- AnalysisResult#tree.value ].

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
