-module(messenger).

-export([start_N_processes/1, start_N_processes/3, process_fun/2, send/3]).

-import(matrix, [ new/1,
		  mget/3,
		  mset/4,
		  map/6,
		  map/2,
		  map_line/3,
		  map_column/3,
		  zeros/1,
		  add_one_to_line/2,
		  add_one_to_column/2,
		  display/1
		]).




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




process_fun (Id, Stamp) ->
	receive
		{send, Destination_Id, Destination_Pid} ->
			io:format ("~p sent a message to ~p~n", [Id, Destination_Id]),
			Destination_Pid ! {msg, Id, Stamp},
			process_fun (Id, Stamp);
		{msg, Sender_Id, Sender_Stamp} ->
			io:format("~p recieved message of ~p. Stamped:~n",
				[Id, Sender_Id]),
			matrix:display(Sender_Stamp),
			process_fun (Id, Stamp)
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
	
	%matrix:display(matrix:add_one_to_column(2, Stamp)).
	
	% process_fun takes a tuple {Process_name, Stamp }
	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp]),
	
	% A global list Messenger_list keeps all the Messenger ids
	start_N_processes (N, Cnt - 1, [{Messenger_Id, Messenger_Pid} | Messenger_List]).

