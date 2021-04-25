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
	E = messenger:start_custom_process (0, [[5, 2, 1], [1, 3, 1], [1, 1, 3]],
		[]),
	E1 = messenger:start_custom_process (1, [[4, 2, 1], [1, 7, 2], [1, 2, 5]],
		E),
	E2 = messenger:start_custom_process (2, [[3, 1, 1], [1, 5, 2], [1, 2, 6]],
		E1),

	messenger:send(0, 2, E2),
	messenger:send(1, 2, 0, 500, E2), % Add an extra delay
	messenger:send(1, 0, E2),
	messenger:send(0, 2, 250, 0, E2),
	messenger:send(0, 1, 250, 0, E2),
	messenger:send(2, 1, 1000, 0, E2).
