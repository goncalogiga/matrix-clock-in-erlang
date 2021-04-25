-module (matrix).

-export([ new/1,
		  init/1,
		  mget/3,
		  mset/4,
		  map/2,
		  zip/3,
		  cmp/5,
		  zeros/1,
		  add_one/3,
		  display/1
		]).




% new (N) -> array
% Type:
% - N: should be a positive integer. It corresponds to the number of
% processes in the system.
%
% new/1 generates a matrix of size N*N that is going to be the matrix clock
% of a process.
new (N) ->
	if N =< 0 ->
		error ("Negative sized matrix.");
	true ->
		array:new(N*N)
	end.



% This function is used localy (in matrix.erl) to find the size of a Matrix
% given by the user as a list of lists.
% Here we simply find the length of a list of elements.
list_len (List) ->
	list_len(List, 0).
list_len ([], Count) ->
	Count;
list_len ([_ | T], Count) ->
	list_len (T, Count + 1).




% This function is used localy (in matrix.erl) to find the size of a Matrix
% given by the user as a list of lists.
% Here we find the length of a list of lists using list_len/1.
matrix_len (Matrix) ->
	matrix_len (Matrix, 0).
matrix_len ([], Count) ->
	Count;
matrix_len ([H | T], Count) ->
	matrix_len (T, Count + list_len (H)).




% This function is used localy (in matrix.erl) to create a Matrix
% with a list of lists.
% Here we associate every sub-list of the list of lists to a line of
% a Matrix.
list_to_line (List, I, N, Matrix) ->
	list_to_line (List, I, N, Matrix, 0).
list_to_line ([], _, N, Matrix, N) ->
	Matrix;
list_to_line ([List_H | List_T], I, N, Matrix, J) ->
	list_to_line (List_T, I, N, array:set(N*I + J, List_H, Matrix), J + 1).




% A simple code to get the value after , in a float.
% from: https://stackoverflow.com/questions/44186796/get-floating-decimal-portion-of-a-float
decimal_point(X, DecimalDigits) when X < 0 ->
  decimal_point(-X, DecimalDigits);
decimal_point(X, DecimalDigits)->
  (X - trunc(X)) * math:pow(10,DecimalDigits).




% init (Matrix_in_List : list of list) -> matrix (ie. array)
% Type:
% - Matrix_in_List: should be a list of two dimensions that can
% generate a Matrix. Exemple: [[1,2,3], [4,5,6], [7,8,9]].
%
% This function creates a matrix (ie. a simple array that can be cut in
% N "slices", representing each line of the matrix) from a list of lists
% given by the user. An example of such a list is given above.
init (Matrix_in_List) ->
	Size = matrix_len (Matrix_in_List),
	Rest = decimal_point (Size, 1),
	if Rest /= 0 ->
		error ("init can only accept square matrices");
	true ->
		N = round(math:sqrt(Size)),
		init (N, Matrix_in_List, 0, matrix:new(N))
	end.
init (N, _, N, Matrix) ->
		Matrix;
init (N, [H | T], I, Matrix) ->
	init (N, T, I + 1, list_to_line (H, I, N, Matrix)).




% A simple function to test the range of coordenates (I,J)
% in a Matrix NxN
check_range(I, J, N) ->
	TestI1 = I >= 0,
	TestI2 = I < N,
	TestJ1 = J >= 0,
	TestJ2 = J < N,
	if TestI1 and TestI2 and TestJ1 and TestJ2 ->
		ok;
	true ->
		error ("Matrix indexes are out of range")
	end.




% mget (I, J, Matrix: array) -> integer
% Type:
% - I: An integer (should be in the range of the matrix)
% - J: An integer (should be in the range of the matrix)
% - Matrix
%
% Gets the element (I,J) of a Matrix. The element is an integer.
mget (I, J, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	check_range (I, J, N),
	array:get(N*I + J, Matrix).




% mset (I, J, Value, Matrix: array) -> matrix (ie. array)
% Type:
% - I: An integer (should be in the range of the matrix)
% - J: An integer (should be in the range of the matrix)
% - Value: An integer
% - Matrix
%
% Sets the element (I,J) of a Matrix to Value. Returns the modified
% matrix.
mset (I, J, Value, Matrix) ->
	N = round(math:sqrt(array:size(Matrix))),
	check_range (I, J, N),
	array:set(N*I + J, Value, Matrix).



% The implementation of the function map/2 explained under this function.
map (_, N, N, Matrix) ->
	Matrix;
map (Fun, K, N, Matrix) ->
	map (Fun, K + 1, N, array:set(K, Fun(array:get(K, Matrix)), Matrix)).




% The implementation of the function zip/3 explained under this function.
zip (_, N, N, Matrix1, _) ->
	Matrix1;
zip (Fun, K, N, Matrix1, Matrix2) ->
	zip (Fun, K + 1, N,
		array:set(K, Fun(array:get(K, Matrix1), array:get(K, Matrix2)),
		Matrix1), Matrix2).




% map (Fun : Function/2, Matrix : matrix (ie. array) -> matrix (ie. array)
% Type:
% - Fun: A function that will be mapped on every element of the matrix
% - Matrix: The target matrix for map/2
%
% map/2 goes through the matrix M and applies the function Fun
% to each member iterated on. Returns the modified matrix.
map (Fun, Matrix) ->
	map (Fun, 0, array:size(Matrix), Matrix).




% zip (Fun : Function, Matrix1 : matrix, Matrix2 : matrix) -> matrix
% Types:
% - Fun : A function taking two arguments and outputing a single
% integer
% - Matrix1: The target matrix for zip/3. This is the matrix that will
% be modified given its elements and the elements of Matrix1.
% - Matrix2: The matrix used for the secound element Y given to function
% Fun.
%
% zip/3 applies the function Fun(X,Y) to each corresponding
% element in both matrixes and creates a new matrix from
% Matrix1 (the target matrix).
% Works similarly to zip() in python; but the output of the
% function applied to each zipped couple directly modifies Matrix1
zip (Fun, Matrix1, Matrix2) ->
	zip (Fun, 0, array:size(Matrix1), Matrix1, Matrix2).




% cmp(Fun: Function, I : integer, J : integer, Matrix1: matrix, Matrix2: matrix
% 	-> Return type of Fun
% Type:
% - Fun: A comparaison function between two integers. This can actualy be of 
% any type; this can also return a new value calculated with I and J if 
% necessary. Idealy though, this should only returns {true} | {false} so the
% name of the function is coherent.
% - I: An integer (inside the Matrix range)
% - J: An integer (inside the Matrix range)
% - Matrix1, Matrix2: The two matrices used
%
% cmp/5 compares the value of Matrix1[i,j] with Matrix2[i,j] with a given
% comparaison function Fun. Outputs the result of Fun.
cmp (Fun, I, J, Matrix1, Matrix2) ->
	SecurityTest = array:size(Matrix1) == array:size(Matrix2),
	if SecurityTest == false ->
		error ("Comparaison between two Matrices of different dimensions");
	true ->
		Val1 = mget(I, J, Matrix1),
		Val2 = mget(I, J, Matrix2),
		Fun (Val1, Val2)
	end.




zero (_) -> 0.




% zeros (Matrix : matrix) -> matrix
%
% zeros/1 fills the matrix Matrix with zeros.
zeros (Matrix) ->
	map (fun zero/1, Matrix).




% add_one(I : integer, J : integer, Matrix : matrix) -> matrix
% Type:
% - I: an integer (inside Matrix range)
% - J: an integer (inside Matrix range)
% - Matrix: the modified matrix
%
% add_one/3 adds one to the element Matrix[i,j] and returns the
% new Matrix.
add_one (I, J, Matrix) ->
	mset(I, J, mget(I, J, Matrix) + 1, Matrix).




% This function is used to transform a format (like in io:format)
% to a string.
% In the description of display/1, the reason this function needs
% to be used is explained
to_string(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).




% The implementation of the function display/1 explained under this function.
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




% display(Matrix : matrix) -> erlang string
% Type:
% - Matrix: a matrix clock
%
% display/1 returns a string nicely displaying a matrix of any size.
% The reason we use to_string and output a string and not directly
% write to stdout is because the display of matrices is concurent if
% multiple processes are displaying their stamps and matrices clocks.
%
% By returning a string we can use a single io:format function, which is
% concurent-safe and output coherent results in the execution of the
% communication between processes.
display(Matrix) ->
	N = array:size(Matrix),
	display (Matrix, 0, round(math:sqrt(N)), "").
