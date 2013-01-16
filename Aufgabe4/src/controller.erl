-module(controller).
-compile(export_all).


start(Args) ->
    spawn(?MODULE, maininit, [Args]).


maininit([Port, TeamNo, StationNo, MulticastIp, LocalIp]) ->
    {ok,MulticastIpTuple} = inet_parse:address(atom_to_list(MulticastIp)),
    {ok,LocalIpTuple} = inet_parse:address(atom_to_list(LocalIp)),
    init(list_to_integer(atom_to_list(Port)),
         list_to_integer(atom_to_list(TeamNo)),
         list_to_integer(atom_to_list(StationNo)),
         MulticastIpTuple,
         LocalIpTuple).


init(Port, TeamNo, StationNo, MulticastIp, LocalIp) ->
    ReceivePort = Port,
    SendPort = StationNo + 14000,
    {ok,ReceiveSocket} = gen_udp:open(ReceivePort, [binary,
                                                   {active, true},
                                                   {multicast_if, LocalIp},
                                                   inet,
                                                   {reuseaddr, true},
                                                   {multicast_loop, true},
                                                   {add_membership, {MulticastIp,LocalIp}}]),
    {ok,SendSocket} = gen_udp:open(SendPort, [binary,
                                             {active, true},
                                             {ip, LocalIp},
                                             inet,
                                             {multicast_loop, true},
                                             {multicast_if, LocalIp}]),

    DataManagerPid = spawn(datamanager, start, [TeamNo, StationNo]),

    SenderPid = spawn(sender, start, [SendSocket, MulticastIp, ReceivePort, self(), DataManagerPid]),
    gen_udp:controlling_process(SendSocket, SenderPid),

    ReceiverPid = spawn(receiver, start, [ReceiveSocket, SenderPid, self(), {TeamNo, StationNo}]),
    gen_udp:controlling_process(ReceiveSocket, ReceiverPid),

    loop(ReceiverPid, SenderPid, DataManagerPid).


loop(RPid, SPid, DMPid) ->
    receive
        {tellMeToSend, Pid, NextSlot} ->
            io:format("Controller\t | received: tellMeToSend~n"),
            SendNow = utilities:get_time_for_next_frame() - utilities:get_timestamp() + (NextSlot * 50) + 10,
            erlang:send_after(SendNow, Pid, sendNow),
            loop(RPid, SPid, DMPid);
        {giveReceiverPid, Pid} ->
            Pid ! {receiverPid, RPid},
            loop(RPid, SPid, DMPid);
        {giveSenderPid, Pid} ->
            Pid ! {SPid},
            loop(RPid, SPid, DMPid);
        {giveDataManagerPid, Pid} ->
            Pid ! {DMPid},
            loop(RPid, SPid, DMPid);
        Any ->
            io:format("Controller\t | received ANY looking like: ~p~n", [Any]),
            loop(RPid, SPid, DMPid)
    end.
