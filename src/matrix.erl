-module (matrix).

-export([ new/1,
		  mget/3,
		  mset/4,
		  map/6,
		  map/2,
		  zip/3,
		  cmp/5,
		  cmp/7,
		  zeros/1,
		  add_one/3,
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




zip (Fun, Min_I, Max_I, Min_J, Max_J, I, J, N, Matrix1, Matrix2) ->
	Test_max = I =< Max_I,
	Test_min = I >= Min_I,

	if I > Max_I ->
		Matrix1;

	I == N ->
		Matrix1;

	Test_min and Test_max ->

		if J < Min_J ->
			zip (Fun, Min_I, Max_I, Min_J, Max_J, I, J + 1,
				 N, Matrix1, Matrix2);
		J > Max_J ->
			zip (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, 0,
				 N, Matrix1, Matrix2);
		J == N ->
			zip (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, 0,
				 N, Matrix1, Matrix2);
		true ->
			Value1 = mget(I, J, Matrix1),
			Value2 = mget(I, J, Matrix2),
			zip (Fun, Min_I, Max_I, Min_J, Max_J, I, J + 1,
				 N, mset(I, J, Fun(Value1, Value2), Matrix1), Matrix2)
		end;

	true ->
		zip (Fun, Min_I, Max_I, Min_J, Max_J, I + 1, J, N, Matrix1, Matrix2)
	end.




map (Fun, Min_I, Max_I, Min_J, Max_J, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, Min_I, Max_I, Min_J, Max_J, 0, 0, N, Matrix).




map (Fun, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	map (Fun, 0, N, 0, N, 0, 0, N, Matrix).




% Applies the function Fun(X,Y) to each corresponding
% element in both matrixes and creates a new matrix from
% the output.
% Works similarly to zip() in python; but the output of the
% function applied to each zipped couple is stored and returned
zip (Fun, Matrix1, Matrix2) ->
	N = round(math:sqrt(array:size(Matrix1))),
	zip (Fun, 0, N, 0, N, 0, 0, N, Matrix1, Matrix2).




% Applies a comparaison function between two elements (in [i,j])
% of two Matrices.
cmp (Fun, I, J, Matrix1, Matrix2) ->
	Val1 = mget(I, J, Matrix1),
	Val2 = mget(I, J, Matrix2),
	Fun (Val1, Val2).




cmp (Fun, I1, J1, I2, J2, Matrix1, Matrix2) ->
	Val1 = mget(I1, J1, Matrix1),
	Val2 = mget(I2, J2, Matrix2),
	Fun (Val1, Val2).




zero (_) -> 0.




zeros (Matrix) ->
	map (fun zero/1, Matrix).




add_one (X) -> X + 1.




add_one (I, J, Matrix) ->
	map (fun add_one/1, I, I, J, J, Matrix).




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

