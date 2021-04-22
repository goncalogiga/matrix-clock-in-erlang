-module(messenger).

-export([start_N_processes/1, process_fun/2, add_event/3, get_events/2, send/2]).




send (Src_id, Pids) ->
	case lists:keyfind(Src_id, 1, Pids) of
		false ->
			error ("In send_msg: destination ID not found.");
		
		{_, Pid} ->
			Pid ! {send}
	end.




add_event (Src_id, Dst_id, Pids) ->
	case lists:keyfind(Dst_id, 1, Pids) of
		false ->
			error ("In add_event (case 1): destination ID not found.");
		
		{Dst_id, Dst_pid} ->
			case lists:keyfind(Src_id, 1, Pids) of
				false ->
					error ("In add_event (case 2): destination ID not found.");
		
				{_, Pid} ->
					Pid ! {event, Dst_id, Dst_pid}
			end
	end.




get_events (Src_id, Pids) ->
	case lists:keyfind(Src_id, 1, Pids) of
		false ->
			error ("In get_events: destination ID not found.");
		
		{_, Pid} ->
			Pid ! {get_events}
	end.






process_fun (ID, Queue_of_Events) ->
	receive
		{get_events} ->
			io:fwrite("Events of process ~p: ~p~n", [ID, Queue_of_Events]),
			process_fun (ID, Queue_of_Events);
        {event, Dst_id, Dst_pid} ->
			process_fun (ID, queue:in({Dst_id, Dst_pid}, Queue_of_Events) );
		{send} ->
			case queue:out(Queue_of_Events) of
				{{_, {Dst_id, Dst_pid}}, Q} ->
					if Dst_id == ID ->
						io:fwrite("~p does a local event~n", [ID]);
					true ->
						io:fwrite("~p sends a message to ~p~n", [ID, Dst_id]),
						Dst_pid ! {msg, ID}
					end,
					process_fun (ID, Q);
				{empty, _ } ->
					io:fwrite("~p has no more messages to send~n", [ID]),
					process_fun (ID, Queue_of_Events)
			end;
		{msg, Src_id} ->
			io:fwrite("~p recieved a message from ~p~n", [ID, Src_id]),
			process_fun (ID, Queue_of_Events)
    end.




start_N_processes (N) ->
	if N =< 1 ->
		error("Not enough processes");
	true ->
		start_N_processes (N, N, [])
	end.




start_N_processes (_, 0, Pids) ->
	Pids;
start_N_processes (N, Ncpy, Pids) ->
	Messenger_id = N - Ncpy,
	Pid = spawn(messenger, process_fun, [Messenger_id, queue:new()]),
	start_N_processes (N, Ncpy - 1, [{Messenger_id, Pid} | Pids]).

