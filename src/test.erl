-module(test).
-export([main/0, main/2, main_w_params/4]).




% This the implementation of the function explained under this function.
main (_, M, M, _) ->
	done;
main (N, M, K, E) ->
	messenger:send(rand:uniform(N) - 1, rand:uniform(N) - 1, rand:uniform(500),
			rand:uniform(50), E),
	main (N, M, K + 1, E).




% This the implementation of the function explained under this function.
main (N, M, K, D1, D2, E) ->
	messenger:send(rand:uniform(N) - 1, rand:uniform(N) - 1, rand:uniform(D1),
		rand:uniform(D2), E),
	main (N, M, K + 1, E).




% main(N : positive integer, M : positive integer)
%
% This function starts N processes with M random communications between them.
% In order for communications to be non-causal, a delay is added in both the
% sending action and the receiving one. See messenger.erl, function send/5, for
% more details.
main (N, M) ->
	E = messenger:start_N_processes (N),
	main (N, M, 0, E).




% main_w_params (N, M, D1, D2: positive integers)
%
% This function can be used to paramater the values
% in rand:uniform(_). D1 is the maximum time an event can take to be sent and
% D2 the maximum time an event can take to be received.
%
% Both of these paramaters will influence the number of desynchronisations that
% happen during the program because the duration of the delays is then chosen
% randomly between [0, D1] and [0, D2].
main_w_params (N, M, D1, D2) ->
	E = messenger:start_N_processes (N),
	main (N, M, 0, D1, D2, E).




% This function can be used to test a specific scenario chosen by the user.
% Here we have the communication explained in the image example.png that can
% be found in the main directory (note: the first local events are not written
% so the display is less heavy.
main () ->
	% Exemple du TD sur les horloges matricielles
	E = messenger:start_custom_process (0, [[5, 2, 1], [1, 3, 1], [1, 1, 3]],
		[]),
	E1 = messenger:start_custom_process (1, [[4, 2, 1], [1, 7, 2], [1, 2, 5]],
		E),
	E2 = messenger:start_custom_process (2, [[3, 1, 1], [1, 5, 2], [1, 2, 6]],
		E1),

	messenger:send(0, 2, E2),
	messenger:send(1, 2, 0, 1000, E2),
	messenger:send(1, 0, E2),
	messenger:send(0, 2, 250, 0, E2),
	messenger:send(0, 1, 250, 0, E2),
	messenger:send(2, 1, 1000, 0, E2).
