-module(test).
-export([main/0, main/2]).




main (_, M, M, _) ->
	done;
main (N, M, K, E) ->
	messenger:send(rand:uniform(N) - 1, rand:uniform(N) - 1, E),
	main (N, M, K + 1, E).




main (N, M) ->
	E = messenger:start_N_processes (N),
	main (N, M, 0, E).




main () ->
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
