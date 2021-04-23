-module(start).

-export([start_N_processes/1, start_N_processes/3]).

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


start_N_processes (N) ->
	if N =< 1 ->
		error("Not enough processes");
	true ->
		start_N_processes (N, N, [])
	end.




start_N_processes (_, 0, Pids) ->
	Pids;
start_N_processes (N, Idx, Pids) ->
	Name  = N - Idx,
	Stamp = matrix:zeros(matrix:new(N)),
	matrix:display(Stamp),
	matrix:display(matrix:add_one_to_column(2, Stamp)).
	%Pid = spawn(messenger, process_fun, [Messenger_id, ]),
	%start_N_processes (N, Ncpy - 1, [{Messenger_id, Pid} | Pids]).

