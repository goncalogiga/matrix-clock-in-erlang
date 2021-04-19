#!/usr/bin/escript -c
%% -*- erlang -*-
%%!-mode(compile)

%execute with :
%   escript -c <filename>

main(_) ->
	Pids = messenger:start_N_processes(4),
	io:fwrite("~p~n", [Pids]),
	messenger:send(0, "cc 0", [Pids]).
	%messenger:send(1, "cc 1", Pids),
	%messenger:send(2, "cc 2", Pids).
