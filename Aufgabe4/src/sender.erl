-module(sender).
-compile(export_all).

start(Socket, Ip, Port, ControllerPid, DMPid) ->
    loop(Socket, Ip, Port, ControllerPid, DMPid).


loop(Socket, Ip, Port, ControllerPid, DMPid) ->
    DMPid ! {next, self()},
    receive
        {nextData, Data} ->
            io:format("sender: got nextData~n"),
            receive
                {sendNow, MySlot} ->
                    io:format("sender: got sendNow~n"),
                    Timestamp = utilities:get_timestamp(),
                    Package = << (list_to_binary(Data))/binary,
                                 MySlot,
                                 Timestamp:64/integer >>,
                    gen_udp:send(Socket, Ip, Port, Package),
                    loop(Socket, Ip, Port, ControllerPid, DMPid)
            end
    end.



