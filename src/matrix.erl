-module (matrix).

-export([ new/1,
		  init/1,
		  mget/3,
		  mset/4,
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




list_len (List) ->
	list_len(List, 0).
list_len ([], Count) ->
	Count;
list_len ([_ | T], Count) ->
	list_len (T, Count + 1).




matrix_len (Matrix) ->
	matrix_len (Matrix, 0).
matrix_len ([], Count) ->
	Count;
matrix_len ([H | T], Count) ->
	matrix_len (T, Count + list_len (H)).




list_to_line (List, I, N, Matrix) ->
	list_to_line (List, I, N, Matrix, 0).
list_to_line ([], _, N, Matrix, N) ->
	Matrix;
list_to_line ([List_H | List_T], I, N, Matrix, J) ->
	list_to_line (List_T, I, N, array:set(N*I + J, List_H, Matrix), J + 1).




init (Matrix_in_List) ->
	N = round (math:sqrt(matrix_len (Matrix_in_List))),
	init (N, Matrix_in_List, 0, matrix:new(N)).
init (N, _, N, Matrix) ->
	Matrix;
init (N, [H | T], I, Matrix) ->
	init (N, T, I + 1, list_to_line (H, I, N, Matrix)).





mget (I, J, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	array:get(N*I + J, Matrix).




mset (I, J, Value, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	array:set(N*I + J, Value, Matrix).




map (_, N, N, Matrix) ->
	Matrix;
map (Fun, K, N, Matrix) ->
	map (Fun, K + 1, N, array:set(K, Fun(array:get(K, Matrix)), Matrix)).




zip (_, N, N, Matrix1, _) ->
	Matrix1;
zip (Fun, K, N, Matrix1, Matrix2) ->
	zip (Fun, K + 1, N,
		array:set(K, Fun(array:get(K, Matrix1), array:get(K, Matrix2)),
		Matrix1), Matrix2).




% Goes through the matrix M, applying the function Fun
% to each member iterated on.
map (Fun, Matrix) ->
	map (Fun, 0, array:size(Matrix), Matrix).




% Applies the function Fun(X,Y) to each corresponding
% element in both matrixes and creates a new matrix from
% the output.
% Works similarly to zip() in python; but the output of the
% function applied to each zipped couple is stored and returned
zip (Fun, Matrix1, Matrix2) ->
	zip (Fun, 0, array:size(Matrix1), Matrix1, Matrix2).




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




add_one (I, J, Matrix) ->
	mset(I, J, mget(I, J, Matrix) + 1, Matrix).




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

