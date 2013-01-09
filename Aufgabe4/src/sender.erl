-module(sender).
-compile(export_all).
-record(sendpackage, {data,
                  slot = 1,
                  time = 0}).


start() ->
    DMPid = spawn(datamanager, start []),
    loop(DMPid).


loop(DMPid) ->
    DMPid ! {next, self()},
    receive
        {nextData, Data} ->
            Slot = getNextFreeSlot(),

ms_time() ->
    {Megasecs, Secs, Microsecs} = erlang:now(),
    ((Megasecs * 1000000 + Secs) * 1000000 + Microsecs) div 1000.


build_sendpackage() ->
    ;


getNextFreeSlot() ->

