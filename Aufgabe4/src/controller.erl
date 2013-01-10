-module(controller).
-export([start/1]).


start([Port, TeamNo, StationNo, MulticastIp, LocalIp]) ->
    {ok,MulticastIpTuple} = inet_parse:address(atom_to_list(MulticastIp)),
    {ok,LocalIpTuple} = inet_parse:address(atom_to_list(LocalIp)),
    init(list_to_integer(atom_to_list(Port)),
         list_to_integer(atom_to_list(TeamNo)),
         list_to_integer(atom_to_list(StationNo)),
         MulticastIpTuple,
         LocalIpTuple).


init(Port, TeamNo, _StationNo, MulticastIp, LocalIp) ->
    ReceivePort = Port,
    SendPort = TeamNo + 14000,
    {ok,ReceiveSocket} = gen_udp:open(ReceivePort, [binary,
                                                  {active, true},
                                                  {multicast_if, LocalIp},
                                                  inet,
                                                  {multicast_loop, false},
                                                  {add_membership,
                                                  {MulticastIp,LocalIp}}]),
    {ok,SendSocket} = gen_udp:open(SendPort, [binary,
                                            {active, true},
                                            {ip, LocalIp},
                                            inet,
                                            {multicast_loop, false},
                                            {multicast_if, LocalIp}]),


    SenderPid = spawn(sender, start, [SendSocket, MulticastIp, ReceivePort, self()]),
    gen_udp:controlling_process(SendSocket, SenderPid),

    ReceiverPid = spawn(receiver, start, [ReceiveSocket, SenderPid, self()]),
    gen_udp:controlling_process(ReceiveSocket, ReceiverPid),

    DataManagerPid = spawn(datamanager, start, []),
    loop(ReceiverPid, SenderPid, DataManagerPid).


loop(RPid, SPid, DMPid) ->
    receive
        {tellMeToSend, Pid, NextSlot} ->
            SendNow = utilities:get_timestamp_for_next_frame() + (NextSlot * 50),
            erlang:send_after(SendNow, Pid, sendNow);
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
            io:format("Received ANY looking like ~p~n", [Any]),
            loop(RPid, SPid, DMPid)
    end.
