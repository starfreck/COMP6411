-module(calling).
-export ([child/0]).

timestamp() ->
    {_,_,Timestamp} = erlang:now(),
    Timestamp.

child() ->
	receive
		{H1,T1} ->
			I = "intro message",
			lists:map(fun(Child) -> Child ! {H1,Child,timestamp(),I} end,T1),
			child();

		{H1,T1,Timestamp,I} ->
			random:seed(),
			timer:sleep(round(timer:seconds(rand:uniform()))),
			main ! {H1,T1,Timestamp,I},
			T1 ! {H1,T1,Timestamp},
			child();

		{H1,T1,Timestamp} ->
			random:seed(),
			timer:sleep(round(timer:seconds(rand:uniform()))),
			RepM = "reply message",
			main ! {H1,T1,Timestamp,RepM,RepM},
			child()
		
	after
		5000 ->
			{_,Name} = erlang:process_info(self(), registered_name),
			io:format("~nProcess ~p has received no calls for 5 second, ending...~n~n",[Name]),
			exit(kill)	
	end.