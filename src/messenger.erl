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




local_event (Id, Stamp) ->
	matrix:add_one (Id, Id, Stamp).




send_event (Id, Sender_Id, Stamp) ->
	matrix:add_one (Id, Sender_Id, matrix:add_one(Id, Id, Stamp)).




check_stamp (Id, Sender_Id, Sender_Stamp, Stamp) ->
	% --- Test n°1 ---
	A = mget(Id, Sender_Id, Sender_Stamp),
	B = mget(Id, Sender_Id, Stamp) + 1,
	if A =:= B ->
		false;
	true ->
		Val = matrix:check_desynchronisation (Id, Sender_Id, Sender_Stamp, Stamp),
		if Val ->
			true;
		true ->
			false
		end
	end.




stamp_update (Stamp, _, N, N) ->
	Stamp;
stamp_update (Stamp, Recieved_Stamp, N, Cnt) ->
	Val1 = array:get(Cnt, Stamp),
	Val2 = array:get(Cnt, Recieved_Stamp),
	if Val1 =< Val2 ->
		stamp_update (array:set(Cnt, Val2, Stamp), Recieved_Stamp, N, Cnt + 1);
	true ->
		stamp_update (Stamp, Recieved_Stamp, N, Cnt + 1)
	end.




process_fun (Id, Stamp) ->
	receive
		{send, Destination_Id, Destination_Pid} ->
			
			% --- Sending a message --- %
			if Id == Destination_Id ->
				New_Stamp = local_event (Id, Stamp),
				io:format ("~p has a local event; stamped:~n~s~n ############################# ~n",
                	[Id, matrix:display(New_Stamp)]),
				process_fun (Id, New_Stamp);
			true ->
				New_Stamp = send_event (Id, Destination_Id, Stamp),
				io:format ("~p sends a message to ~p; stamped:~n~s~n ##################################### ~n",
					[Id, Destination_Id, matrix:display(New_Stamp)]),
				Destination_Pid ! {msg, Id, New_Stamp},
				process_fun (Id, New_Stamp)
			end;

		{msg, Sender_Id, Sender_Stamp} ->
			Is_stamp_valid = check_stamp (Id, Sender_Id, Sender_Stamp, Stamp),
			% --- Receiving a message => Checking for desynchronisation ---
			if Is_stamp_valid == false ->
				% Case of desynchronisation
				io:format("~p received a message from ~p [desynchronised !!]; local stamp of ~p: ~n~s~n received stamp of ~p:~n~s~n #################################### ~n",
					[Id, Sender_Id, Id, matrix:display(Stamp), Sender_Id, matrix:display(Sender_Stamp)]),
				process_fun (Id, Stamp);

			true ->
				New_Stamp1 = matrix:add_one (Id, Id, Stamp),
				New_Stamp2 = matrix:add_one (Sender_Id, Id, New_Stamp1),
				New_Stamp3 = stamp_update (New_Stamp2, Sender_Stamp,
					array:size(Stamp), 0),
				io:format("~p received a message from ~p; local stamp of ~p: ~n~s~n received stamp of ~p:~n~s~n updated stamp of ~p: ~n~s~n ######################################## ~n",
					[Id, Sender_Id, Id, matrix:display(Stamp), Sender_Id,
					matrix:display(Sender_Stamp), Id,
					matrix:display(New_Stamp3)]),
				process_fun (Id, New_Stamp3)
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
	
	%matrix:display(matrix:add_one_to_column(2, Stamp)).
	
	% process_fun takes a tuple {Process_name, Stamp }
	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp]),
	
	% A global list Messenger_list keeps all the Messenger ids
	start_N_processes (N, Cnt - 1, [{Messenger_Id, Messenger_Pid} | Messenger_List]).

