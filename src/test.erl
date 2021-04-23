-module(test).
-export([main/0]).

main() ->
	% Exemple du TD sur les horloges matricielles
	E = messenger:start_N_processes(3),
	messenger:send(0, 1, E),
	messenger:send(0, 2, E),
	messenger:send(0, 2, E),
	messenger:send(0, 1, E),
	messenger:send(1, 1, E),
	messenger:send(1, 2, E),
	messenger:send(1, 0, E),
	messenger:send(2, 2, E),
	messenger:send(2, 0, E).
