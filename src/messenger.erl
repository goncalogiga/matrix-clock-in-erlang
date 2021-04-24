-module(messenger).

-export([start_N_processes/1, start_N_processes/3, process_fun/3, send/3]).

-define (L, "has a local event").
-define (S, "sends a message to").
-define (R, "received a message from").
-define (D, "DESYNCHRONIZED").
-define (LSO, "Local Stamp of").
-define (RSO, "Received Stamp of").
-define (USO, "Updated Stamp of").
-define (DELIMITER, "========================================================").




send (Sender_Id, Destination_Id, Messenger_List) ->
	case lists:keyfind(Sender_Id, 1, Messenger_List) of
		false ->
			error ("In send/3: sender not found.");
		
		{_, Sender_Pid} ->
			case lists:keyfind (Destination_Id, 1, Messenger_List) of
				false ->
					error ("In send/3: destination not found");
				{_, Destination_Pid} ->
					Sender_Pid ! {send, Destination_Id, Destination_Pid}
			end
	end.




local_event (Id, Stamp) ->
	matrix:add_one (Id, Id, Stamp).




send_event (Id, Sender_Id, Stamp) ->
	matrix:add_one (Id, Sender_Id, matrix:add_one(Id, Id, Stamp)).




process_fun_send (Id, Stamp, Destination_Id, Destination_Pid) ->
	% --- Sending a message --- %
	
	if Id == Destination_Id ->
		New_Stamp = local_event (Id, Stamp),

		% DISPLAY ------------------------------------------------------------
		io:format ("~p ~s; stamped:~n~s~s~n",
    		[Id, ?L, matrix:display(New_Stamp), ?DELIMITER]),
		% --------------------------------------------------------------------

		New_Stamp;
	
	true ->
		New_Stamp = send_event (Id, Destination_Id, Stamp),

		% DISPLAY ------------------------------------------------------------
		io:format ("~p ~s ~p; stamped:~n~s~s~n",
			[Id, ?S, Destination_Id, matrix:display(New_Stamp), ?DELIMITER]),
		% --------------------------------------------------------------------

		% [Note] For a run without non-causal messages uncomment the following line
		% and comment the erlang:send_after one.
		% Destination_Pid ! {msg, Id, New_Stamp},
		erlang:send_after (rand:uniform(1000), Destination_Pid, {msg, Id, New_Stamp}),
		New_Stamp
	end.




process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, Is_Dump) ->
	Is_stamp_valid = check_stamp:check_stamp (Id, Sender_Id, Sender_Stamp, Stamp),
	
	% --- Receiving a message => Checking for desynchronisation ---
	if Is_stamp_valid == false ->
		% Case of desynchronisation

		% DISPLAY ------------------------------------------------------------
		if Is_Dump == 0 ->
			io:format("~p ~s ~p ~s; ~s ~p: ~n~s~n ~s ~p:~n~s~s~n",
				[Id, ?R, Sender_Id, ?D, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized};
		true ->
			{Stamp, desynchronized}
		% --------------------------------------------------------------------
		end;

	true ->
		New_Stamp1 = matrix:add_one (Id, Id, Stamp),
		New_Stamp2 = matrix:add_one (Sender_Id, Id, New_Stamp1),
		New_Stamp3 = matrix:zip (fun max/2, New_Stamp2, Sender_Stamp),

		% DISPLAY ------------------------------------------------------------
		if Is_Dump == 0 ->
			io:format("~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok};
		true ->
			io:format("(DELAYED) ~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok}
		end
		% --------------------------------------------------------------------
	end.




process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp) ->
	process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 0).




dump_buffer (_, Stamp, [], Buffer) ->
	{Stamp, Buffer};
dump_buffer (Id, Stamp, [{Sender_Id, Sender_Stamp} | Buf_T], Buf_Heads) ->
	case process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 1) of
		{_, desynchronized} ->
			dump_buffer (Id, Stamp, Buf_T,
				[ {Sender_Id, Sender_Stamp} | Buf_Heads ]);
		{New_Stamp, ok} ->
			{New_Stamp, Buf_Heads ++ Buf_T}
	end.




dump_buffer (Id, Stamp, Buffer) ->
	dump_buffer (Id, Stamp, Buffer, []).




process_fun (Id, Stamp, Buffer) ->
	receive

		{send, Destination_Id, Destination_Pid} ->
			process_fun (Id, process_fun_send (Id, Stamp, Destination_Id,
				Destination_Pid), Buffer);

		{msg, Sender_Id, Sender_Stamp} ->
			{Dumped_Stamp, Buf_Remains} = dump_buffer (Id, Stamp, Buffer),
			case process_fun_msg (Id, Dumped_Stamp, Sender_Id, Sender_Stamp) of
				{_, desynchronized} ->
					process_fun (Id, Dumped_Stamp,
						[{Sender_Id, Sender_Stamp} | Buf_Remains]);

				{New_Stamp, ok} ->
					process_fun (Id, New_Stamp, Buf_Remains)
			end
	end.




start_N_processes (N) ->
	if N =< 1 ->
		error("Not enough processes");
	true ->
		start_N_processes (N, N, [])
	end.



start_N_processes (_, 0, Messenger_List) ->
	Messenger_List;
start_N_processes (N, Cnt, Messenger_List) ->
	Messenger_Id  = N - Cnt,
	Stamp = matrix:zeros(matrix:new(N)),
	
	% process_fun takes a tuple {Process_name, Stamp }
	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp, []]),

	% A global list Messenger_list keeps all the Messenger ids
	start_N_processes (N, Cnt - 1, [{Messenger_Id, Messenger_Pid} | Messenger_List]).

