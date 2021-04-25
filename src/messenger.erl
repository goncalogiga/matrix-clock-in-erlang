-module(messenger).

-export([start_N_processes/1, start_N_processes/3, start_custom_process/3,
		 process_fun/3, send/3, send/5]).


% Because the execution of N processes means that there is concurancy
% between processes; the display of information about a given process
% needs to be entirly encapsulated inside one call to io:format.
%
% This makes the calls to io:format very messy and long. The following
% macros are an attempt to make the calls to io:format less ugly.
%
-define (L, "has a local event").
-define (S, "sends a message to").
-define (R, "received a message from").
-define (D, "DESYNCHRONIZED").
-define (LSO, "Local Stamp of").
-define (RSO, "Received Stamp of").
-define (USO, "Updated Stamp of").
-define (DELIMITER, "========================================================").




% ~~~NOTE~~~: In every comment, the type ID is used. This is not a type
% per-say because it is only an integer; yet every time ID is written
% it means that this integer represents a process wich is nammed ID.
%
% If there are N processes, ID can range from 0 to N-1. Each ID is linked
% to a PID of the process in a global list that you can see being used in
% test.erl. This list is always called Messenger_List in this file.
%
% This means that each of our N processes has a name, ID, which is just an
% integer representing the ID-th process.




% This is the description of both send/3 and send/5.
%
% send (Sender_Id : ID, Destination_Id: ID, Messenger_List)
% Type:
% - For an explanation of the types, read the note at the begining of this
% file.
%
% send/3 or send/5 is used to make a process of id Sender_Id, send a message to
% the process of id Destination_Id. When a process is created, it waits for
% a message {send, ...} wich tells it to send a message. This is the function
% that sends such {send, ...} directives and makes processes communicate with
% each other.
%
% Because erlang manages concurancy well, send/5 is introduced.
% send/5 is the same as send/3 but with two extra arguments: Send_Delay and
% Receive_Delay. These are integers that will be evaluate as microsecounds.
%
% send/5 can therefore be used to manualy create desynchronisations between our
% processes and test the correction done by our matrix clock mechanism.
%
% Send_Delay makes so that a process is informed it needs to send a message only
% after a given delay.
%
% Receive_Delay makes so that a process can send a message but this message is
% only delivered after a given delay (but it is sent right away if Send_Delay
% is 0.
send (Sender_Id, Destination_Id, Messenger_List) ->
	send (Sender_Id, Destination_Id, 0, 0, Messenger_List).
send (Sender_Id, Destination_Id, Send_Delay, Receive_Delay, Messenger_List) ->
	case lists:keyfind(Sender_Id, 1, Messenger_List) of
		false ->
			error ("In send/3: sender not found.");
		
		{_, Sender_Pid} ->
			case lists:keyfind (Destination_Id, 1, Messenger_List) of
				false ->
					error ("In send/3: destination not found");
				{_, Destination_Pid} ->
					if Send_Delay == 0 ->
						Sender_Pid ! {send, Destination_Id, Destination_Pid,
							Receive_Delay};
					true ->
						erlang:send_after (Send_Delay, Sender_Pid,
							{send, Destination_Id, Destination_Pid, Receive_Delay})
					end
			end
	end.



% local_event (Id : ID, Stamp: matrix) -> matrix
% Type:
% - For an explanation of the types, read the note at the begining of this
% file.
%
% local_event/2 is used when a process has a local event.
% For the matrix clock to work, this means that its internal
% clock (here Stamp) needs to be increased at the (Id,Id) position.
% This function applies these changes to Stamp.
local_event (Id, Stamp) ->
	matrix:add_one (Id, Id, Stamp).




% send_event (Id : ID, Sender_Id: ID, Stamp: matrix) -> matrix
% Type:
% - For an explanation of the types, read the note at the begining of this
% file.
%
% send_event/3 is used when process Id sends a message to Sender_Id.
% For the matrix clock to work, this means that the internal clock
% of Id (here Stamp) needs to be increased at the (Id,Id) position and
% at the (Id, Sender_Id) position.
% This function applies these changes to Stamp.
send_event (Id, Sender_Id, Stamp) ->
	matrix:add_one (Id, Sender_Id, matrix:add_one(Id, Id, Stamp)).




% process_fun_send (Id: ID, Stamp: matrix, Destination_Id: ID, Destination_Pid
% 	: PID, Delay: integer representing microsecounds)
% Type:
% - For an explanation of the types, read the note at the begining of this
% file.
%
% process_fun_send/5 is one of the two "sub-functions" of process_fun
% (process_fun is explained under)
% This function deals with the receiving of a sending directive (Id needs to
% send a message to Destination_Id). It performs the necessary modifications in
% the matrix clock (Stamp) and displays these modifications.
%
% If Delay != 0, the actual action of sending the message to Destination_Id is
% delayed (see send/5 for more details)
process_fun_send (Id, Stamp, Destination_Id, Destination_Pid, Delay) ->
	% --- Sending a message --- %
	
	if Id == Destination_Id ->
		New_Stamp = local_event (Id, Stamp),

		% DISPLAY ------------------------------------------------------------
		io:format ("~p ~s; stamped:~n~s~s~n",
    		[Id, ?L, matrix:display(New_Stamp), ?DELIMITER]),
		% --------------------------------------------------------------------

		New_Stamp;
	
	true ->
		New_Stamp = send_event (Id, Destination_Id, Stamp),

		% DISPLAY ------------------------------------------------------------
		io:format ("~p ~s ~p; ~s ~p: ~n~s~n sent updated stamp: ~n~s~s~n",
			[Id, ?S, Destination_Id, ?LSO, Id, matrix:display(Stamp),
				matrix:display(New_Stamp), ?DELIMITER]),
		% --------------------------------------------------------------------

		if Delay == 0 ->
			Destination_Pid ! {msg, Id, New_Stamp},
			New_Stamp;
		true ->
		   erlang:send_after (rand:uniform(Delay), Destination_Pid,
				{msg, Id, New_Stamp}),
			New_Stamp
		end
	end.




% process_fun_msg (Id: ID, Stamp: matrix, Sender_Id: ID, Sender_Stamp : matrix
% 	Is_Delayed: boolean)
% Type:
% - For an explanation of the types, read the note at the begining of this
% file.
%
% process_fun_msg/5 is one of the two "sub-functions" of process_fun
% (process_fun is explained under)
% This function deals with the case of receiving a message from a Sender (ie.
% Id receives a message from Sender_Id) In order for the matrix clock to work,
% this function calls the function check_stamp/4 written in check_stamp.erl to
% test if the message is non-causal. If the message is non-causal, the clock of
% Id is not updated. Otherwise, the clock of Id is updated (the case true in
% the if of this function)
%
% We separate Test1 and Test2 in order to display the reason why the message is
% non-causal.
%
% The last argument Is_Delayed is explained under the function
% receive_delayed_msgs/3
process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, Is_Delayed) ->
	{Test1, Test2, K, I} = check_stamp:check_stamp (Id, Sender_Id, Sender_Stamp, Stamp),
	
	% --- Receiving a message => Checking for desynchronisation ---
	if Test1 == false ->

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p [(EM[~p,~p] != HM[~p,~p] + 1) => ~s]~n ~s ~p: ~n~s~n ~s ~p:~n~s~s~n",
				[Id, ?R, Sender_Id, Sender_Id, Id, Sender_Id, id, ?D, ?LSO, Id,
				matrix:display(Stamp), ?RSO, Sender_Id,
				matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized};
		true ->
			io:format("~p tested bufferized stamp from ~p: ~s ~p ~n~s~n ~s ~p ~n~s~n (still non-causal.) ~n~s~n",
				[Id, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized}
		% --------------------------------------------------------------------
		end;

	Test2 == false ->

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p [(EM[~p,~p] > HM[~p,~p]) => ~s]~n ~s ~p: ~n~s~n ~s ~p:~n~s~s~n",
				[Id, ?R, Sender_Id, K, I, K, I, ?D, ?LSO, Id,
				matrix:display(Stamp), ?RSO, Sender_Id,
				matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized};
		true ->
			io:format("~p tested bufferized stamp from ~p: ~s ~p ~n~s~n ~s ~p ~n~s~n (still non-causal.) ~n~s~n",
				[Id, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?DELIMITER]),
			{Stamp, desynchronized}
		% --------------------------------------------------------------------
		end;

	true ->
		New_Stamp1 = matrix:add_one (Id, Id, Stamp),
		New_Stamp2 = matrix:add_one (Sender_Id, Id, New_Stamp1),
		New_Stamp3 = matrix:zip (fun max/2, New_Stamp2, Sender_Stamp),

		% DISPLAY ------------------------------------------------------------
		if Is_Delayed == 0 ->
			io:format("~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok};
		true ->
			io:format("(SENT WITH DELAY) ~p ~s ~p; ~s ~p: ~n~s~n ~s ~p:~n~s~n ~s ~p: ~n~s~s~n",
				[Id, ?R, Sender_Id, ?LSO, Id, matrix:display(Stamp), ?RSO,
				Sender_Id, matrix:display(Sender_Stamp), ?USO, Id,
				matrix:display(New_Stamp3), ?DELIMITER]),
				{New_Stamp3, ok}
		end
		% --------------------------------------------------------------------
	end.




% This a simple "overload" of the function process_fun_msg/5 explained above.
% This makes so the paramater Is_Delayed (explained in the comment under this
% one) is 0 by default.
process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp) ->
	process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 0).




% receive_delayed_msgs (Id: ID, Stamp: matrix: FIFO: queue) -> {matrix, queue}
% - Type:
%  FIFO: this argument is an erlang queue. This is a first in first out queue
%  that keeps all the messages that need to be delayed (because they failed to
%  verify the two conditions tested upon the receival of a message by
%  a process) in an order such that the oldest delayed message modifies the
%  Stamp first.
%
% receive_delayed_msgs/3 is the function that sends messages that were received
% in a way that would make the communication between processes non-causal. The
% queue FIFO is stored by each process and it gets filled with messages that
% did not pass the two tests explained above. Every time a message is received
% by Id, this function is called and every element stored in FIFO gets tested
% again. If the stored elements are now valid, they are sent.
%
% Using the paramater Is_Delayed in process_fun_msg, the clock is updated with
% a different format (ie. printed to the terminal differently) than the normaly
% updated clocks; so we now this message was received too early and is now sent
% because the mecanism of the matrix clock corrected it's sending.

receive_delayed_msgs (Id, Stamp, FIFO) ->
	receive_delayed_msgs (Id, Stamp, FIFO, queue:new()).




% This function implements the mecanism explained above, in the description of
% the receive_delayed_msgs/3. The reason for receive_delayed_msgs/4 is because
% we go through the list of delayed messages and it can happen that a message
% in this queue is sent before the elements before it. This means that each element
% that could not be sent, needs to be stored to a queue DESYNC_FIFO so we never
% pop elements (and loose them) that will not actualy be sent after.
receive_delayed_msgs (Id, Stamp, FIFO, DESYNC_FIFO) ->
	Test = queue:is_empty(FIFO),
	if Test == true ->
		{Stamp, DESYNC_FIFO};
	true ->
		{{_, {Sender_Id, Sender_Stamp}}, New_FIFO} = queue:out(FIFO),
		case process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp, 1) of
			{_, desynchronized} ->
				receive_delayed_msgs (Id, Stamp, New_FIFO,
					queue:in({Sender_Id, Sender_Stamp}, DESYNC_FIFO));
			{New_Stamp, ok} ->
				receive_delayed_msgs (Id, New_Stamp, New_FIFO)
		end
	end.




% process_fun (Id : ID, Stamp : matrix, FIFO: queue) ->
% Types:
%  - The "type" ID and matrix are explained in the note at the begining of this
%  file.
%  - FIFO: this is a queue that stores each {Sender_Id, Sender_Stamp} (ie: the
%  process name and the stamp it sent to Id) that did not pass the two tests
%  done in check_stamp.erl. Read receive_delayed_msgs/3 to understand how this
%  queue is used to send the delayed elements at the right time.
%
%  process_fun/3 is the function ran by every process. It is divided in two
%  "sub-functions", process_fun_send/5 and process_fun_msg/4 explained above.
%  process_fun/3 waits for the user directive to send a message via {send, ...}
%  and sends the messages to the other processes while checking for
%  desynchronisation.
process_fun (Id, Stamp, FIFO) ->
	receive

		{send, Destination_Id, Destination_Pid, Delay} ->
			process_fun (Id,
				process_fun_send (Id, Stamp, Destination_Id,
					Destination_Pid, Delay), FIFO);

		{msg, Sender_Id, Sender_Stamp} ->
			case process_fun_msg (Id, Stamp, Sender_Id, Sender_Stamp) of

				{_, desynchronized} ->
					process_fun (Id, Stamp, queue:in({Sender_Id, Sender_Stamp},
						FIFO));

				{New_Stamp, ok} ->
					% Check if any delayed message is now ready to be sent:
					{New_Stamp2, New_FIFO} = receive_delayed_msgs (Id, New_Stamp,
						FIFO),
					process_fun (Id, New_Stamp2, New_FIFO)
			end
	end.




% start_N_processes (N : integer)
%
% This function starts N processes that execute the function process_fun with
% an Id (the i-th itteration of the function start_process/3), a matrix clock
% full of zeros and a new queue (used to send delayed messages)
start_N_processes (N) ->
	if N =< 1 ->
		error("Not enough processes");
	true ->
		start_N_processes (N, N, [])
	end.




% This is the implementation of the function explained just above.
start_N_processes (_, 0, Messenger_List) ->
	Messenger_List;
start_N_processes (N, Cnt, Messenger_List) ->
	Messenger_Id  = N - Cnt,
	Stamp = matrix:zeros(matrix:new(N)),
	
	% process_fun takes a tuple {Process_name, Stamp, Queue}
	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp,
		queue:new()]),

	% A global list Messenger_list keeps all the Messenger ids
	start_N_processes (N, Cnt - 1, 
		[{Messenger_Id, Messenger_Pid} | Messenger_List]).




% start_custom_process (Messenger_Id: ID, Matrix_in_List : erlang list,
% 	Messenger_List)
%
% start_custom_process/3 is used to start a process with more control over the
% marix clock than in the general function start_N_processes/1.
%
% Therefore, the initial state of the matrix clock can be given to this function
% via a simple list of lists (example: [[1,2],[2,1]])
%
% This function outputs the Messenger_List which needs to be shared by all
% functions that create and ask processes to send messages.
start_custom_process (Messenger_Id, Matrix_in_List, Messenger_List) ->
	Stamp = matrix:init(Matrix_in_List),

	Messenger_Pid = spawn (messenger, process_fun, [Messenger_Id, Stamp,
		queue:new()]),

	[ {Messenger_Id, Messenger_Pid} | Messenger_List ].
