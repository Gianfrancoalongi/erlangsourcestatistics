-module(erlslim).
-export([start/0]).
-define(VERSION,"0.3").
-record(data,{lsock, asock, request, result, reply, bye}).

start() -> 
    Steps = [fun set_log_path_and_reset_log_file/0,
	     fun log_start_of_service/0,
	     fun open_listening_socket/0,
	     fun accept_incoming_connection/1,
	     fun send_slim_protocol_version/1,
	     fun receive_commands_and_send_replies_until_bye/1,
	     fun close_connections/1,
	     fun log_end_of_service/0,
	     fun exit_with_code_zero/0
	    ],
    seq:run_(Steps).

set_log_path_and_reset_log_file() ->
    erlslim_log:set_log_path(init:get_argument('slim_log')),
    erlslim_log:reset_log_file().

log_start_of_service() ->
    erlslim_log:log(erlslim_server, started).

open_listening_socket() ->
    {ok, [[Port]]} = init:get_argument('slim_port'),
    IPort = list_to_integer(Port),
    {ok, LSock} = gen_tcp:listen(IPort,[{active,false},{reuseaddr,true}]),
    #data{lsock = LSock}.

accept_incoming_connection(Data) ->    
    {ok, ASock} = gen_tcp:accept(Data#data.lsock),
    Data#data{asock = ASock}.

send_slim_protocol_version(Data) ->
    gen_tcp:send(Data#data.asock,"Slim -- V"++?VERSION++"\n"),
    Data.

receive_commands_and_send_replies_until_bye(Data) ->
    Steps = [ fun() -> Data end,
	      fun receive_slim_request/1,
	      fun handle_request/1,
	      fun send_response/1,
	      fun loop_or_end_on_bye/1
	    ],
    seq:run_(Steps).

receive_slim_request(Data) ->
    {ok, [A,B,C,D,E,F]=TotalSize} = gen_tcp:recv(Data#data.asock, 6),
    {ok,_} = gen_tcp:recv(Data#data.asock,1),
    Bytes = list_to_integer(TotalSize),
    {ok, Received} = gen_tcp:recv(Data#data.asock, Bytes),
    Data#data{request = [A,B,C,D,E,F,$:|Received],
	      bye = Received == "bye"
	     }.

handle_request(Data) ->
    Res = erlslim_command:execute(erlslim_decoder:decode(Data#data.request)),
    Data#data{result = Res}.

send_response(Data) ->
    EncodedReply = erlslim_encoder:encode(Data#data.result),
    gen_tcp:send(Data#data.asock, EncodedReply),
    Data#data{reply = EncodedReply}.

loop_or_end_on_bye(Data) when Data#data.bye ->
    Data;
loop_or_end_on_bye(Data) ->
    receive_commands_and_send_replies_until_bye(Data).

close_connections(Data) ->
    gen_tcp:close(Data#data.asock),
    gen_tcp:close(Data#data.lsock).

log_end_of_service() ->
    erlslim_log:log(erlslim_server, stopped).

exit_with_code_zero() ->
    exit(0).  
