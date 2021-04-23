-module (matrix).

-export([ new/1,
		  mget/3,
		  mset/4,
		  map/6,
		  map/2,
		  map_line/3,
		  map_column/3,
		  zeros/1,
		  add_one/3,
		  add_one_to_line/2,
		  add_one_to_column/2,
		  check_desynchronisation/4,
		  display/1,
		  nonformated_display/1
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




add_one (I, J, Matrix) ->
	map (fun add_one/1, I, I, J, J, Matrix).



add_one_to_line (Line, Matrix) ->
	map_line (fun add_one/1, Line, Matrix).




add_one_to_column (Col, Matrix) ->
	map_column (fun add_one/1, Col, Matrix).




check_desynchronisation (Excluded_I, Excluded_J, MatrixEM, MatrixHM) ->
	N = round(math:sqrt(array:size(MatrixEM))),
	check_desynchronisation (Excluded_I, Excluded_J, MatrixEM, MatrixHM, N, 0).




check_desynchronisation (_, _, _, _, N, N) ->
	true;
check_desynchronisation (Excluded_I, Excluded_J, MatrixEM, MatrixHM, N, Cnt) ->
	I = round(Cnt/N),
	J = Cnt rem N,
	Test1 = Excluded_I == I,
	Test2 = Excluded_J == J,

	if Test1 or Test2 ->
		check_desynchronisation (Excluded_I, Excluded_J, MatrixEM, MatrixHM, N,
			Cnt + 1);
	true ->
		Val1 = mget(Excluded_I, I,MatrixEM),
		Val2 = mget(Excluded_J, J,MatrixHM),
		if Val1 =< Val2 ->
			check_desynchronisation (Excluded_I, Excluded_J, MatrixEM,
				MatrixHM, N, Cnt + 1);
		true ->
			false
		end
	end.




to_string(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).




display (Matrix, Cnt, N, String) ->
	if
		Cnt == N*N - 1 ->
			String ++ to_string("~p]]\n", [array:get(Cnt, Matrix)]);
		Cnt == 0 ->
			display (Matrix, Cnt + 1, N,
				String ++ to_string("[[~p,", [array:get(Cnt, Matrix)]));
		Cnt == 1 ->
			display(Matrix, Cnt + 1, N,
				String ++ to_string("~p,", [array:get(Cnt, Matrix)]));
		Cnt rem N == N - 1 ->
			display (Matrix, Cnt + 1, N,
				String ++ to_string("~p],\n", [array:get(Cnt, Matrix)]));
		Cnt rem N == 0 ->
			display (Matrix, Cnt + 1, N,
				String ++ to_string(" [~p,",  [array:get(Cnt, Matrix)]));
		true ->
			display (Matrix, Cnt + 1, N,
				String ++ to_string("~p,",   [array:get(Cnt, Matrix)]))
	end.




display(Matrix) ->
	N = array:size(Matrix),
	display (Matrix, 0, round(math:sqrt(N)), "").




nonformated_display (Matrix) ->
	io:format("~p~n", [Matrix]).

