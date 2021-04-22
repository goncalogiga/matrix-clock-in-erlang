#!/usr/bin/escript -c
%% -*- erlang -*-
%%!-mode(compile)

%execute with :
%   escript -c <filename>

main(_) ->
	% Exemple du TD sur les horloges matricielles
	E = messenger:start_N_processes(3),
	messenger:add_event(0, 0, E),
	messenger:add_event(0, 2, E),
	messenger:add_event(0, 2, E),
	messenger:add_event(0, 1, E),
	messenger:add_event(1, 1, E),
	messenger:add_event(1, 2, E),
	messenger:add_event(1, 0, E),
	messenger:add_event(2, 2, E),
	messenger:add_event(2, 0, E),

	io:fwrite("~p~n", [messenger:send(0,E)]),
	io:fwrite("~p~n", [messenger:send(0,E)]),
	io:fwrite("~p~n", [messenger:send(0,E)]),
	io:fwrite("~p~n", [messenger:send(0,E)]),
	io:fwrite("~p~n", [messenger:send(1,E)]),
	io:fwrite("~p~n", [messenger:send(1,E)]),
	io:fwrite("~p~n", [messenger:send(1,E)]),
	io:fwrite("~p~n", [messenger:send(2,E)]),
	io:fwrite("~p~n", [messenger:send(2,E)]).
