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
    {_,List} = file:consult("calls.txt"),
    io:format("****Calls to be made ****~n"),
    printList(List),
    % Sending each recoard line-by-line
    lists:map(fun(Line) ->
        {Name,ContactList} = Line,
        register(Name,spawn(calling,child,[])),
        Name ! {self(),{Name,ContactList}} end, 
    List),
    main().

% Master Method
main()->
	receive
        % Print Message from Child
        {Name,Friend,Timestamp,Message} ->
        	io:format("~p received ~s  from ~p [~p] ~n",[Name,Message,Friend,Timestamp]),
			main()
	after
        10000 ->
            io:format("~nMaster has received no replies for 10 seconds, ending...~n")
	end.