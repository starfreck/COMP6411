% master process
 
-module(exchange).
-export([start/0,main/0]).




start() -> 
    % Reading file to Map
    {ok, List} = file:consult("calls.txt"),
    io:format("****Calls to be made ****~n"),
    printList(List),
    % Start New Thread For Master Process
    register(list_to_atom("main"),spawn(exchange, main, [])),
    % Sending each recoard line-by-line to Main 
    lists:map(fun(Line) -> main ! {Line} end, List),
    io:fwrite("\n").

printList([]) ->
    io:fwrite("\n");

printList([H|T]) ->	
	{H1,T1} = H,
	io:format("~p:~p~n",[H1,T1]),
	printList(T).

killChildren(List) ->
    lists:map(fun(X) -> {S,_} = X, S ! stop end, List),
    timer:sleep(1).

main()->
	receive
        
		{Line} ->
			{H1,T1} = Line,
			register(H1,spawn(calling,child,[])),
			H1 ! {H1,T1},
			main();   

		{H1,T1,Timestamp,I} ->
			io:format("~p received ~p  from ~p [~p] ~n",[T1,I,H1,Timestamp]),
			main();

		{H1,T1,Timestamp,RepM,RepM} ->
			io:format("~p received ~p from ~p [~p]~n",[H1,RepM,T1,Timestamp]),
			main()

	after
        10000 ->
            io:format("~nMaster has received no replies for 10 seconds, ending...~n"),
            exit(kill)
	end.