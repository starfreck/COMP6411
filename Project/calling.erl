-module(calling).
-export ([child/0]).

timestamp() ->
    {_,_,Timestamp} = erlang:now(),
    Timestamp.

child() ->
	receive
		{Name,ContactList} ->
			lists:map(fun(Friend) -> Friend ! {Name,Friend,timestamp(),"intro message"} end, ContactList),
			child();

		{Name,Friend,Timestamp,Message} ->
			timer:sleep(rand:uniform(100)),
			main ! {Name,Friend,Timestamp,Message},
			Friend ! {Name,Friend,Timestamp},
			child();

		{Name,Friend,Timestamp} ->
			timer:sleep(rand:uniform(100)),
			% Swaping Friend and Name
			main ! {Friend,Name,Timestamp,"reply message"},
			child()
		
	after
		5000 ->
			{_,Name} = erlang:process_info(self(), registered_name),
			io:format("~nProcess ~p has received no calls for 5 second, ending...~n~n",[Name])	
	end.