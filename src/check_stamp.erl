-module (check_stamp).

-export([check_stamp/4]).



% leq (A, B) -> {true} | {false}
leq (A, B) -> A =< B.




% This function applies the idea behnid check_stamp_test_2/4.
% It adds an extra counter K that ranges from 0 to N-1 (every line of the
% stamps) and tests the condition explained under.
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




% check_stamp_test_2 (I, J, Matrix1, Matrix2) -> {true, 0, 0} | {false, K, I}
% Types:
% - I: An integer; should be the Id of the process stamped with Matrix2
% - J: An integer; should be the Id of the process stamped with Matrix1
% - Matrix1: A matrix representing the stamp of Process J (sender)
% - Matrix2: A matrix representing the stamp of Process I (receiver)
%
% check_stamp_test_2/4 is a function used to test if EM[k,i] =< HM[k,i] for
% each k such that k != i and k != j.
%
% This is the secound condition that is tested after a process receives
% a message.
%
% In order to have a clear display of what is going on, this function returns
% {false, K, I} in case the condition is not satisfied; so EM[k,i] > HM[k,i]
% gets printed.
check_stamp_test_2 (I, J, EM, HM) ->
	N = round(math:sqrt(array:size(EM))),
	check_stamp_test_2 (0, N, I, J, EM, HM).




% check_stamp_fun (A, B) -> {true} | {false}
check_stamp_fun (A, B) -> A == B + 1.




% check_stamp (Id, Sender_Id, Sender_Stamp, Stamp) -> {true, true, 0, 0}
% | {true, false, K, I}
% | {false, false, K, I}
% Types:
% - Id: The id of the process receiving a message.
% - Sender_Id: the id of the process sending a message.
% - Sender_Stamp: the stamp of the process sending a message.
% - Stamp: the stamp of the process receiving a message.
%
% check_stamp/4 checks the two conditions necessary for a Stamp to be updated
% after it received a message.
%
% Test1 is a simple call to matrix:cmp (see matrix.erl) that compares the
% squares [Sender_Id, Id] (ie. [j,i]) and tests the check_stamp_fun which
% gives us the result to the first test.
%
% Test2 is calculated with the function check_stamp_test_2, which tests every
% line of the stamps (except i and j) and verifies that the i-th column of
% Stamp is always lower than Sender_Stamp. If not a desynchronisation happened.
%
% The [K,I] square that failed in Test2 is returned for the display.
check_stamp (Id, Sender_Id, Sender_Stamp, Stamp) ->
	Test1 = matrix:cmp(fun check_stamp_fun/2, Sender_Id, Id, Sender_Stamp,
		Stamp),

	{Test2, K, I} = check_stamp_test_2 (Id, Sender_Id, Sender_Stamp, Stamp),

	{Test1, Test2, K, I}.

