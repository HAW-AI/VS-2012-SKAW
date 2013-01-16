-module(receiver).
-compile(export_all).


start(Socket, SenderPid, ControllerPid, TeamInfo) ->
    {S1, S2, S3} = erlang:now(),
    random:seed(S1, S2, S3),
    io:format("Receiver\t | start~n"),
    gen_udp:controlling_process(Socket, self()),
    %%TODO: Next slot = 19 ist schlecht wenn mehrere eigene Stationen im Netz
    TimeForNextFrame = utilities:get_time_for_next_frame() - utilities:get_timestamp(),
    erlang:send_after(TimeForNextFrame + 1000,
                      self(),
                      startToSend),
    erlang:send_after(TimeForNextFrame,
                      self(),
                      newFrame),
    initloop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo).


initloop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo) ->
    receive
        {udp, _ReceiveSocket, _IP, _InPortNo, Package} ->
            io:format("Receiver\t | received: new package~n"),
            {handler, NewFreeSlots, NewUsedSlots, _, _} = handle_received_package(Package, FreeSlots, UsedSlots, MyLastSlot, TeamInfo),
            initloop(Socket, NewFreeSlots, NewUsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo);
        startToSend ->
            io:format("Receiver\t | received: startToSend at: ~p~n", [utilities:get_timestamp()]),
            erlang:send_after(998, self(), newFrame),
            io:format("Receiver\t | DEBUG -- Initloop - FreeSlotList: ~p~n", [FreeSlots]),
            erlang:send_after(50*lists:nth(1, FreeSlots) + 5, self(), sendNow),
            loop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, MyLastSlot, TeamInfo, 0);
        newFrame ->
            initloop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo)
        end.


loop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo, AlreadySent) ->
    receive
        {udp, _ReceiveSocket, _IP, _InPortNo, Package} ->
            io:format("Receiver\t | received: new package~n"),
            {handler, NewFreeSlots, NewUsedSlots, NewMyLastSlot, State} = handle_received_package(Package, FreeSlots, UsedSlots, MyLastSlot, TeamInfo),
            io:format("Receiver\t | DEBUG -- MainLoop - FreeSlotlist: ~p~n", [NewFreeSlots]),
            case State of
                init ->
                    erlang:send_after(1000, self(), reset),
                    resetloop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo);
                _ ->
                    loop(Socket, NewFreeSlots, NewUsedSlots, SenderPid, ControllerPid, NewMyLastSlot, TeamInfo, AlreadySent)
            end;
        sendNow when AlreadySent =:= 0 ->
            io:format("Receiver\t | received: sendNow~n"),
            io:format("Receiver\t | DEBUG -- Mainloop - FreeSlotList: ~p~n", [FreeSlots]),
            SenderPid ! {sendNow, lists:nth(1, FreeSlots)},
            ControllerPid ! {tellMeToSend, self(), lists:nth(1, FreeSlots)},
            loop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo, 1);
        sendNow ->
            ControllerPid ! {tellMeToSend, self(), lists:nth(1, FreeSlots)},
            loop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo, AlreadySent);
        newFrame ->
            io:format("Receiver\t | DEBUG -- MainLoop - received newFrame at: ~p~n", [utilities:get_timestamp()]),
            SendAfter = utilities:get_time_for_next_frame() - utilities:get_timestamp() - 1,
            erlang:send_after(SendAfter, self(), newFrame),
            loop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo, 0)
        end.


resetloop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, MyLastSlot, TeamInfo) ->
    receive
        reset ->
            set_initial_timers(),
            initloop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo);
        newFrame ->
            resetloop(Socket, werkzeug:shuffle(lists:seq(0,19)), [], SenderPid, ControllerPid, -1, TeamInfo);
        {udp, _ReceiveSocket, _IP, _InPortNo, Package} ->
            io:format("Receiver\t | received: new package~n"),
            {handler, NewFreeSlots, NewUsedSlots, _, _} = handle_received_package(Package, FreeSlots, UsedSlots, MyLastSlot, TeamInfo),
            resetloop(Socket, NewFreeSlots, NewUsedSlots, SenderPid, ControllerPid, -1, TeamInfo);
        _ ->
            resetloop(Socket, FreeSlots, UsedSlots, SenderPid, ControllerPid, -1, TeamInfo)
    end.


myFlush() ->
    receive
        _ -> myFlush()
    after 0 -> ok
    end.


set_initial_timers() ->
    TimeForNextFrame = utilities:get_time_for_next_frame() - utilities:get_timestamp(),
    erlang:send_after(TimeForNextFrame + 1000,
                      self(),
                      startToSend),
    erlang:send_after(TimeForNextFrame,
                      self(),
                      newFrame).


handle_received_package(Package, FreeSlots, UsedSlots, MyLastSlot, TeamInfo) ->
    {TeamNo, StationNo, WishedSlot, _Data, _SentTime} = utilities:match_message(Package),
    ReceivedTime = utilities:get_timestamp(),
    ReceivedSlot = utilities:get_slot_for_msec(ReceivedTime),
    io:format("Receiver\t | received -- slot: ~p | time: ~p | Package: ~p~n", [ReceivedSlot,
                                                                  ReceivedTime,
                                                                  utilities:message_to_string(Package)]),
    io:format("Receiver\t | DEBUG -- handle_received_package - received for package above: TeamNo: ~p | StationNo: ~p | WishedSlot: ~p~n", [TeamNo, StationNo, WishedSlot]),
    case TeamInfo of
        {TeamNo, StationNo} ->
            NewMyLastSlot = ReceivedSlot,
            NewFreeSlots = FreeSlots;
        _ ->
            NewMyLastSlot = MyLastSlot,
            NewFreeSlots = lists:delete(WishedSlot, FreeSlots)
    end,

    Collided = lists:member(ReceivedSlot, UsedSlots),
    if
        Collided ->
            case NewMyLastSlot of
                ReceivedSlot ->
                    io:format("Receiver\t | Own station involved in collision in slot: ~p~n", [NewMyLastSlot]),
                    State = init;
                _ ->
                    io:format("Receiver\t | Collision detected in slot: ~p~n", [ReceivedSlot]),
                    State = loop
            end;
        true ->
            State = loop
    end,
    NewUsedSlots = UsedSlots ++ [ReceivedSlot],
    {handler, NewFreeSlots, NewUsedSlots, NewMyLastSlot, State}.


