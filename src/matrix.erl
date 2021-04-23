-module (matrix).

-export([ new/1,
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




new (N) ->
	if N =< 0 ->
		error ("Negative sized matrix.");
	true ->
		array:new(N*N)
	end.




mget (I, J, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	array:get(N*I + J, Matrix).




mset (I, J, Value, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	array:set(N*I + J, Value, Matrix).




% Goes through the matrix M, applying the function Fun
% to each member iterated on.
map (Fun, Min_I, Max_I, Min_J, Max_J, I, J, N, Matrix) ->
	Test_max = I =< Max_I,
	Test_min = I >= Min_I,

	if I > Max_I ->
		Matrix;

	I == N ->
		Matrix;

	Test_min and Test_max ->

		if J < Min_J ->
			map (Fun, Min_I, Max_I, Min_J, Max_J, I, J + 1,
				 N, Matrix);
		J > Max_J ->
			map (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, 0,
				 N, Matrix);
		J == N ->
			map (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, 0,
				 N, Matrix);
		true ->
			Value = mget(I, J, Matrix),
			map (Fun, Min_I, Max_I, Min_J, Max_J, I, J + 1,
				 N, mset(I, J, Fun(Value), Matrix))
		end;

	true ->
		map (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, J, N, Matrix)
	end.




map (Fun, Min_I, Max_I, Min_J, Max_J, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, Min_I, Max_I, Min_J, Max_J, 0, 0, N, Matrix).




map (Fun, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, 0, N, 0, N, 0, 0, N, Matrix).




map_line (Fun, Line, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, Line, Line, 0, N, Matrix).




map_column (Fun, Col, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, 0, N, Col, Col, Matrix).




zero (_) -> 0.




zeros (Matrix) ->
	map (fun zero/1, Matrix).




add_one (X) -> X + 1.




add_one_to_line (Line, Matrix) ->
	map_line (fun add_one/1, Line, Matrix).




add_one_to_column (Col, Matrix) ->
	map_column (fun add_one/1, Col, Matrix).




display (Matrix, Cnt, N) ->
	if
		Cnt == N*N - 1 ->
			io:fwrite("~p]]~n", [array:get(Cnt, Matrix)]),
			done;
		Cnt == 0 ->
			io:fwrite("[[~p,", [array:get(Cnt, Matrix)]),
			display (Matrix, Cnt + 1, N);
		Cnt == 1 ->
			io:fwrite("~p,",   [array:get(Cnt, Matrix)]),
			display (Matrix, Cnt + 1, N);
		Cnt rem N == N - 1 ->
			io:fwrite("~p],~n", [array:get(Cnt, Matrix)]),
			display (Matrix, Cnt + 1, N);
		Cnt rem N == 0 ->
			io:fwrite(" [~p,",  [array:get(Cnt, Matrix)]),
			display (Matrix, Cnt + 1, N);
		true ->
			io:fwrite("~p,",   [array:get(Cnt, Matrix)]),
			display (Matrix, Cnt + 1, N)
	end.




display(Matrix) ->
	N = array:size(Matrix),
	display (Matrix, 0, round(math:sqrt(N))).

