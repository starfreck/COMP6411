% master process
-module(exchange).
-export([start/0,main/0]).

% Print the List with recursion
printList([]) ->
    io:fwrite("\n");

printList([Head|Tail]) ->	
	{Name,ContactList} = Head,
	io:format("~p:~p~n",[Name,ContactList]),
	printList(Tail).

start() -> 
    % Reading file
    {ok, List} = file:consult("calls.txt"),
    io:format("****Calls to be made ****~n"),
    printList(List),
    % Start New Thread For Master Process
    register(list_to_atom("main"),spawn(exchange, main, [])),
    % Sending each recoard line-by-line to Main 
    lists:map(fun(Line) -> 
                    {Name,ContactList} = Line,
		            register(Name,spawn(calling,child,[])),
		            Name ! {Name,ContactList} end, 
                List),
    main().

% Master Method
main()->
	receive
        {Name,Friend,Timestamp,Message} ->
        	io:format("~p received ~s  from ~p [~p] ~n",[Friend,Message,Name,Timestamp]),
			main()
	after
        10000 ->
            io:format("~nMaster has received no replies for 10 seconds, ending...~n")
	end.