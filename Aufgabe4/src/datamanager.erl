-module(datamanager).
-compile(export_all).


start_pid() ->
    Pid = erlang:spawn(?MODULE, start, []),
    Pid.


start() ->
    Port = open_port({spawn, "./DataSource 02 11"}, []),
    DataQueue = queue:new(),
    loop(Port, DataQueue).


loop(Port, DataQueue) ->
    receive
        {next, Pid} ->
            {{value, Item}, NewDataQueue} = queue:out(DataQueue),
            Pid ! {nextData, Item},
            loop(Port, NewDataQueue);
        {Port, {data, Data}} ->
            NewDataQueue = queue:in(Data, DataQueue),
            loop(Port, NewDataQueue);
        kill ->
            io:format("Received kill~n");
        Any ->
            io:format("Received any data looking like: ~p~n", [Any]),
            loop(Port, DataQueue)
    end.


