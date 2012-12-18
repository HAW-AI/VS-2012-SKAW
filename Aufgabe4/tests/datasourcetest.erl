%% THIS IS JUST A TESTFILE
-module(datasourcetest).
-compile(export_all). %% just for testing pruposes
-record(package, {data,
                  slot = 1,
                  time = binary:encode_unsigned(1317977394825, big)}).
                  %% TODO: Get time as 8 Byte integer!
                  %%       Right now it is only 6 Bytes, no clue why!


%% Matches the Binary created after receiving the Data of DataSource to 
%% Name (Name of the Team + Stationnr. eg: "team 02-11")
%% Data (Data received from DataSource)
%% NextSlot (NextSlot used to send data: here hardcoded to "1" for testing only)
%% Time (Send-Time as UNIX-Time)
match(Bin) ->
    <<Name:10/binary,
      Data:14/binary,
      NextSlot:1/binary,
      Time:6/binary,
      _Rest/binary>> = Bin,
    io:format("Name: ~p -- Data: ~p -- Slot: ~p -- Time: ~p~n",
                                             [binary_to_list(Name),
                                              binary_to_list(Data),
                                              binary_to_list(NextSlot),
                                              binary_to_list(Time)]).
    %% TODO: Right now logging transforms the given lists to characters
    %%       which just looks like crap. Needs fixing!
    %werkzeug:logging(test, "Name: " ++ binary_to_list(Name)
    %                        ++ " | Data: " ++ binary_to_list(Data)
    %                        ++ " | Slot: " ++ binary_to_list(NextSlot)
    %                        ++ " | Time: " ++ binary_to_list(Time)).


%% Opens a port to the spawned executable "DataSource"
%% to receive everything send by this source
start() ->
    %% For "open_port()" to work the DataSource executable need to be placed
    %% in the same directory as this file.
    %% Alternatively use absolute path to DataSource
    Port = open_port({spawn, "./DataSource 02 11"}, []),
    do_read(Port, #package{}).


%% Reads ONE TIME from the datasource
%%
%% {Port, eof} is not needed at the moment
%%
%% to read multiple lines from DataSource:
%% comment out: "port_close()" and os:cmd("killall DataSource")
%% comment in: "do_read()"
do_read(Port, P) ->
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            os:cmd("killall DataSource"), %% since "port_close()" does not kill
                                          %% the started process use this cmd
                                          %% (UNIX only...i guess)
            io:format("Received: ~p on port: ~p~n", [Data, Port]),
            match(Bin = build_package(P#package{data = Data})),
            Bin;
            %do_read(Port, P);
        {Port, eof} ->
            io:format("Received eof ~n");
        Any ->
            io:format("Received any data looking like: ~p~n", [Any]),
            do_read(Port, P)
    end.


%% Builds the complete binary package which can be sent
build_package(P) ->
    Bin = list_to_binary([P#package.data, P#package.slot, P#package.time]),
    % io:format("Bin: ~p~n", [Bin]),
    Bin.
