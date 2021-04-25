-module (check_stamp).

-export([check_stamp/4]).




leq (A, B) -> A =< B.




check_stamp_test_2 (N, N, _, _, _, _) ->
	{true, 0, 0};
check_stamp_test_2 (K, N, I, J, EM, HM) ->
	Test1 = K == I,
	Test2 = K == J,
	if Test1 or Test2 ->
		check_stamp_test_2 (K + 1, N, I, J, EM, HM);
	true ->
		Val = matrix:cmp (fun leq/2, K, I, EM, HM),
		if Val == false ->
			{false, K, I};
		true ->
			check_stamp_test_2 (K + 1, N, I, J, EM, HM)
		end
	end.




check_stamp_test_2 (I, J, EM, HM) ->
	N = round(math:sqrt(array:size(EM))),
	check_stamp_test_2 (0, N, I, J, EM, HM).




check_stamp_fun (A, B) -> A == B + 1.




check_stamp (Id, Sender_Id, Sender_Stamp, Stamp) ->
	Test1 = matrix:cmp(fun check_stamp_fun/2, Sender_Id, Id, Sender_Stamp,
		Stamp),

	{Test2, K, I} = check_stamp_test_2 (Id, Sender_Id, Sender_Stamp, Stamp),

	{Test1, Test2, K, I}.

