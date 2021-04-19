-module(messenger).

-export([start_N_processes/1, start_process/0, send/2]).




send (Dst_id, Msg) ->
	case lists:keymember(Dst_id, 2, Pids) of
		true ->
			Dst_id ! Msg;
		false ->
			error("Destinition not found.")
	end.



start_process () ->
	receive
        Msg ->
            io:format("~p~n", Msg)
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
	Pid = spawn(messenger, start_process, []),
	Messenger_id = N - Ncpy,
	start_N_processes (N, Ncpy - 1, [{Messenger_id, Pid} | Pids]).

