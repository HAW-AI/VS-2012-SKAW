-module(receiver).
-compile(export_all).


start(Socket, SenderPid, ControllerPid) ->
    gen_udp:controlling_process(Socket, self()),
    %%TODO: Next slot = 19 ist schlecht wenn mehrere eigene Stationen im Netz
    TimeForNextFrame = utilities:get_time_for_next_frame(),
    erlang:send_after(TimeForNextFrame + 1000,
                      startToSend,
                      self()),
    erlang:send_after(TimeForNextFrame - utilities:get_timestamp(),
                      newFrame,
                      self()),
    initloop(Socket, lists:seq(0,19), SenderPid, ControllerPid).


initloop(Socket, FreeSlots, SenderPid, ControllerPid) ->
    receive
        {udp, _ReceiveSocket, _IP, _InPortNo, Package} ->
            {TeamnNo, StationNo, UsedSlot, Data, SentTime} = utilities:match_message(Package),
            SentSlot = utilities:get_slot_for_msec(SentTime),
            NewFreeSlots = lists:delete(UsedSlot, FreeSlots),
            loop(Socket, NewFreeSlots, SenderPid, ControllerPid);
        startToSend ->
            erlang:send_after(newFrame, self(), 1000),
            ControllerPid ! {tellMeToSend, self(), lists:nth(1, FreeSlots)},
            loop(Socket, lists:seq(0,19), SenderPid, ControllerPid);
        newFrame ->
            initloop(Socket, lists:seq(0,19), SenderPid, ControllerPid)
        end.


loop(Socket, FreeSlots, SenderPid, ControllerPid) ->
    receive
        {udp, _ReceiveSocket, _IP, _InPortNo, Package} ->
            {TeamnNo, StationNo, UsedSlot, Data, SentTime} = utilities:match_message(Package),
            SentSlot = utilities:get_slot_for_msec(SentTime),
            NewFreeSlots = lists:delete(UsedSlot, FreeSlots),
            loop(Socket, NewFreeSlots, SenderPid, ControllerPid);
        sendNow ->
            SenderPid ! {sendNow, lists:nth(1, FreeSlots)},
            ControllerPid ! {tellMeToSend, self(), lists:nth(1, FreeSlots)};
        newFrame ->
            erlang:send_after(1000, newFrame, self()),
            loop(Socket, [], SenderPid, ControllerPid)
        end.


