-module(messenger).

-export([start_N_processes/1, start_N_processes/3, start_custom_process/3,
		 process_fun/3, send/3, send/5]).

-define (L, "has a local event").
-define (S, "sends a message to").
-define (R, "received a message from").
-define (D, "DESYNCHRONIZED").
-define (LSO, "Local Stamp of").
-define (RSO, "Received Stamp of").
-define (USO, "Updated Stamp of").
-define (DELIMITER, "========================================================").




send (Sender_Id, Destination_Id, Messenger_List) ->
	send (Sender_Id, Destination_Id, 0, 0, Messenger_List).




send (Sender_Id, Destination_Id, Send_Delay, Receive_Delay, Messenger_List) ->
	case lists:keyfind(Sender_Id, 1, Messenger_List) of
		false ->
			error ("In send/3: sender not found.");
		
		{_, Sender_Pid} ->
			case lists:keyfind (Destination_Id, 1, Messenger_List) of
				false ->
					error ("In send/3: destination not found");
				{_, Destination_Pid} ->
					if Send_Delay == 0 ->
						Sender_Pid ! {send, Destination_Id, Destination_Pid,
							Receive_Delay};
					true ->
						erlang:send_after (Send_Delay, Sender_Pid,
							{send, Destination_Id, Destination_Pid, Receive_Delay})
					end
			end
	end.




local_event (Id, Stamp) ->
	matrix:add_one (Id, Id, Stamp).




send_event (Id, Sender_Id, Stamp) ->
	matrix:add_one (Id, Sender_Id, matrix:add_one(Id, Id, Stamp)).




process_fun_send (Id, Stamp, Destination_Id, Destination_Pid, Delay) ->
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
		io:format ("~p ~s ~p; ~s ~p: ~n~s~n sent updated stamp: ~n~s~s~n",
			[Id, ?S, Destination_Id, ?LSO, Id, matrix:display(Stamp),
				matrix:display(New_Stamp), ?DELIMITER]),
		% --------------------------------------------------------------------

		if Delay == 0 ->
			Destination_Pid ! {msg, Id, New_Stamp},
			New_Stamp;
		true ->
		   erlang:send_after (rand:uniform(Delay), Destination_Pid,
				{msg, Id, New_Stamp}),
			New_Stamp
		end
	end.




process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, Is_Delayed) ->
	{Test1, Test2, K, I} = check_stamp:check_stamp (Id, Sender_Id, Sender_Stamp, Stamp),
	
	% --- Receiving a message => Checking for desynchronisation ---
	if Test1 == false ->

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p [(EM[~p,~p] != HM[~p,~p] + 1) => ~s]~n ~s ~p: ~n~s~n ~s ~p:~n~s~s~n",
				[Id, ?R, Sender_Id, Sender_Id, Id, Sender_Id, id, ?D, ?LSO, Id,
				matrix:display(Stamp), ?RSO, Sender_Id,
				matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized};
		true ->
			io:format("~p tested bufferized stamp from ~p: ~s ~p ~n~s~n ~s ~p ~n~s~n (still non-causal.) ~n~s~n",
				[Id, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized}
		% --------------------------------------------------------------------
		end;

	Test2 == false ->

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p [(EM[~p,~p] > HM[~p,~p]) => ~s]~n ~s ~p: ~n~s~n ~s ~p:~n~s~s~n",
				[Id, ?R, Sender_Id, K, I, K, I, ?D, ?LSO, Id,
				matrix:display(Stamp), ?RSO, Sender_Id,
				matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized};
		true ->
			io:format("~p tested bufferized stamp from ~p: ~s ~p ~n~s~n ~s ~p ~n~s~n (still non-causal.) ~n~s~n",
				[Id, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized}
		% --------------------------------------------------------------------
		end;

	true ->
		New_Stamp1 = matrix:add_one (Id, Id, Stamp),
		New_Stamp2 = matrix:add_one (Sender_Id, Id, New_Stamp1),
		New_Stamp3 = matrix:zip (fun max/2, New_Stamp2, Sender_Stamp),

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok};
		true ->
			io:format("(SENT WITH DELAY) ~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok}
		end
		% --------------------------------------------------------------------
	end.




process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp) ->
	process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 0).




receive_delayed_msgs (Id, Stamp, FIFO) ->
	receive_delayed_msgs (Id, Stamp, FIFO, queue:new()).




receive_delayed_msgs (Id, Stamp, FIFO, DESYNC_FIFO) ->
	Test = queue:is_empty(FIFO),
	if Test == true ->
		{Stamp, DESYNC_FIFO};
	true ->
		{{_, {Sender_Id, Sender_Stamp}}, New_FIFO} = queue:out(FIFO),
		case process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 1) of
			{_, desynchronized} ->
				receive_delayed_msgs (Id, Stamp, New_FIFO,
					queue:in({Sender_Id, Sender_Stamp}, DESYNC_FIFO));
			{New_Stamp, ok} ->
				receive_delayed_msgs (Id, New_Stamp, New_FIFO)
		end
	end.




process_fun (Id, Stamp, FIFO) ->
	receive

		{send, Destination_Id, Destination_Pid, Delay} ->
			process_fun (Id,
				process_fun_send (Id, Stamp, Destination_Id,
					Destination_Pid, Delay), FIFO);

		{msg, Sender_Id, Sender_Stamp} ->
			case process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp) of

				{_, desynchronized} ->
					process_fun (Id, Stamp, queue:in({Sender_Id, Sender_Stamp},
						FIFO));

				{New_Stamp, ok} ->
					{New_Stamp2, New_FIFO} = receive_delayed_msgs (Id, New_Stamp,
						FIFO),
					process_fun (Id, New_Stamp2, New_FIFO)
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
	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp,
		queue:new()]),

	% A global list Messenger_list keeps all the Messenger ids
	start_N_processes (N, Cnt - 1, 
		[{Messenger_Id, Messenger_Pid} | Messenger_List]).




start_custom_process (Messenger_Id, Matrix_in_List, Messenger_List) ->
	Stamp = matrix:init(Matrix_in_List),

	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp,
		queue:new()]),

	[ {Messenger_Id, Messenger_Pid} | Messenger_List ].
