-module(sender).
-compile(export_all).


start(Socket, Ip, Port, ControllerPid) ->
    DMPid = spawn(datamanager, start, []),
    loop(Socket, Ip, Port, ControllerPid, DMPid).


loop(Socket, Ip, Port, _ControllerPid, DMPid) ->
    DMPid ! {next, self()},
    receive
        {nextData, Data} ->
            receive
                {sendNow, MySlot} ->
                    Timestamp = utilities:get_timestamp(),
                    Package = << (list_to_binary(Data))/binary,
                                 MySlot,
                                 Timestamp:64/integer >>,
                    gen_udp:send(Socket, Ip, Port, Package)
            end
    end.



