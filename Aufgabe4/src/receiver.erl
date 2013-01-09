-module(receiver).
-compile(export_all).


start(Socket) ->
    gen_udp:controlling_process(Socket, self()),
    loop(Socket, lists:seq(0,19)).


loop(Socket, FreeSlots) ->
    receive
        {udp, _ReceiveSocket, _IP, _InPortNo, Packet} ->
            NewFreeSlots = lists:delete(packageHandler:getSlotWish(Packet),
                                        FreeSlots),
            loop(Socket, NewFreeSlots);

