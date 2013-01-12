-module(datamanager).
-compile(export_all).


start_pid() ->
    Pid = erlang:spawn(?MODULE, start, []),
    Pid.


start(TeamNo, StationNo) ->
    DataSource = "./DataSource " ++ integer_to_list(TeamNo) ++ " " ++ integer_to_list(StationNo),
    Port = open_port({spawn, DataSource}, []),
    DataQueue = queue:new(),
    receive
        {Port, {data, Data}} ->
            NewDataQueue = queue:in(Data, DataQueue),
            loop(Port, NewDataQueue)
    end.


loop(Port, DataQueue) ->
    receive
        {next, Pid} ->
            io:format("DataManager\t | received: next~n"),
            {{value, Item}, NewDataQueue} = queue:out(DataQueue),
            Pid ! {nextData, Item},
            loop(Port, NewDataQueue);
        {Port, {data, Data}} ->
            NewDataQueue = queue:in(Data, DataQueue),
            loop(Port, NewDataQueue);
        kill ->
            os:cmd("killall DataSource"),
            io:format("Received kill~n");
        Any ->
            io:format("DataManager\t | received any data looking like: ~p~n", [Any]),
            loop(Port, DataQueue)
    end.


