-module(calling).
-export ([child/0]).

timestamp() ->
    {_,_,Timestamp} = erlang:now(),
    Timestamp.

child() ->
	receive
		{Sender,{Name,ContactList}} ->
			lists:map(fun(Friend) -> Friend ! {Sender,{Friend,Name,timestamp(),"intro message"}} end, ContactList),
			child();

		{Sender,{Name,Friend,Timestamp,Message}} ->
			timer:sleep(rand:uniform(100)),
			Sender ! {Name,Friend,Timestamp,Message},
			Friend ! {Sender,{Friend,Name,Timestamp}},
			child();

		{Sender,{Name,Friend,Timestamp}} ->
			timer:sleep(rand:uniform(100)),
			Sender ! {Name,Friend,Timestamp,"reply message"},
			child()

	after
		5000 ->
			{_,Name} = erlang:process_info(self(), registered_name),
			io:format("~nProcess ~p has received no calls for 5 second, ending...~n~n",[Name])	
	end.