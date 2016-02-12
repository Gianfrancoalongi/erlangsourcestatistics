-module(ess_graphics).
-include("ess.hrl").
-export([generate/1]).

-export([t/0, gen_res/0]).


gen_res() ->
    Res = [ess:dir("/local/scratch/etxpell/proj/sgc/src/sgc/reg/src",[], "sbg_inc.conf"),
           ess:dir("/local/scratch/etxpell/proj/sgc/src/sgc/oab/src/",[], "sbg_inc.conf"),
           ess:dir("/local/scratch/etxpell/proj/sgc/src/sgc/b2b/src/",[], "sbg_inc.conf")],
    file:write_file("./res.data", term_to_binary(Res)).

t() ->
    {ok,Bin}  = file:read_file("./res.data"),
    Res = binary_to_term(Bin),
    generate(Res).

generate(AnalysisResults) ->
    Tags = [T || {T, _} <- (hd(AnalysisResults))#tree.value ],
    
    [ generate_for_one_tag(AnalysisResults, Tag) || Tag <- Tags ].


generate_for_one_tag(AnalysisResults, Tag) ->
    RawData = [ {get_block_name(Dir), gv(Tag, Value)} || 
                  #tree{name = Dir, value = Value} <- AnalysisResults ],
    
    DataPoints =  generate_datapoints(RawData),
    MaxY = 5+maximum_average(RawData),
    
    Header = capitalize(a2l(Tag)),
    HTML = generate_html(Header, MaxY, DataPoints),
    
    DstDir = "/home/etxpell/dev_patches",
    FileName = filename:join(DstDir, Tag)++".html",
    file:write_file(FileName, HTML).

maximum_average(RawData) ->
    lists:max([ element(3,element(2,D)) || D <- RawData ]).

generate_datapoints(RawData) ->
    lists:map(fun generate_datapoint/1, RawData).

generate_datapoint({Block,{Max,_,Avg}}) ->
    io_lib:format("{ y: ~p, z: ~p, label:\"~s\"}", [Avg, Max, Block]).

get_block_name(Dir) ->
    [_,Block|_] = lists:reverse(filename:split(Dir)),
    Block.

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



generate_html(Header, MaxY, DataPoints) ->
"<!DOCTYPE HTML>
<html>

<head>  
  <script type=\"text/javascript\">
  window.onload = function () {
    var chart = new CanvasJS.Chart(\"chartContainer\",
    {
      zoomEnabled: true,
      animationEnabled: true,
      title:{
        text: \""++Header++"\"                    
      },
      axisX: {
        title:\"Block\",
        labelAngle: -30       
      },
      axisY:{
        title: \""++Header++"\",              
        gridThickness: 1,
        tickThickness: 1,
        gridColor: \"lightgrey\",
        tickColor: \"lightgrey\",
        lineThickness: 0,
        valueFormatString:\"#.\",
        maximum: "++i2l(MaxY)++",
        interval: 1        
      },

      data: [
      {        
        type: \"bubble\",     
        toolTipContent: \"<span style='\\\"'color: {color};'\\\"'><strong>{label}</strong></span><br/> <strong>Max "++Header++"</strong> {z} <br/> <strong>Mean "++Header++"</strong> {y} <br/>\",
        dataPoints: 
        ["
++string:join(DataPoints,",\n")++" 
        ]
      }
      ]
    });

chart.render();
}
</script>
<script type=\"text/javascript\" src=\"http://canvasjs.com/assets/script/canvasjs.min.js\"></script>
</head>
<body>
  <div id=\"chartContainer\" style=\"height: 300px; width: 100%;\">
  </div>
</body>

</html>".

