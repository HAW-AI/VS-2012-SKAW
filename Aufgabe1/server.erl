-module(server).
-export([start/4, start_with_cfg/0]).
-import(tools).
-author("Sebastian Krome, Andreas Wimmer").


%% Starts the server with values of given config file
start_with_cfg() ->
    {Lifetime,
     Remembertime,
     Servername,
     DLQ_Limit,
     _Difftime} = tools:getServerConfigData(),
    start(Servername, DLQ_Limit, Lifetime*1000, Remembertime*1000).


%% Public start function
start(Name,MaxDelivery,Lifetime, Remembertime) ->
    VPid = verwaltung:start(Remembertime),
    Pid = spawn(fun() -> loop(0,
                              dict:new(),
                              dict:new(),
                              MaxDelivery,
                              VPid,
                              Lifetime)
            end),
    register(Name, Pid).


%% Main loop waiting for messages
loop(MsgNumber,Delivery,Holdback,MaxDelivery,VPid,Lifetime) ->
    io:format("Serverstart_Delivery: ~p~n",[dict:size(Delivery)]),
    io:format("Serverstart_Holdback: ~p~n",[dict:size(Holdback)]),
    io:format("MsgNumber: ~p~n",[MsgNumber]),
    {ok,TRef} = timer:send_after(Lifetime, self(), die),
    receive
        {getmsgid, Pid} ->
            log(serverLog(MsgNumber, number, Pid)),
            Pid ! MsgNumber;

        {dropmessage, {Nachricht, Number}} ->
            {NewDelivery, NewHoldback} = drpMsg(Nachricht,
                                                Number,
                                                Delivery,
                                                Holdback,
                                                MaxDelivery),
            timer:cancel(TRef),
            loop(MsgNumber,NewDelivery,NewHoldback,MaxDelivery, VPid,Lifetime);
        {getmessages, CPid} ->
            getMessages(CPid,VPid,Delivery),
            timer:cancel(TRef),
            loop(MsgNumber,Delivery,Holdback,MaxDelivery,VPid,Lifetime);
        die ->
            VPid ! kill,
            io:format("bye~n"),
            erlang:exit(normal)
    end,
    timer:cancel(TRef),
    loop(MsgNumber+1,Delivery,Holdback,MaxDelivery,VPid,Lifetime).


%% Logging function
%% Returns (bsp: "server: Nummer: 12 an <0.40.0> gesendet")
serverLog(MsgNumber, number, Pid) ->
    "server: Nummer: "
    ++integer_to_list(MsgNumber)
    ++" an "
    ++pid_to_list(Pid)
    ++" gesendet --- "
    ++now_to_list()
    ++"\n".


%% Converts the current time to a list
%% Returns current time as a list
now_to_list() ->
    {_,{Hour, Minutes, Seconds}} = erlang:localtime(),
    integer_to_list(Hour)
    ++":"
    ++integer_to_list(Minutes)
    ++":"
    ++integer_to_list(Seconds).


%% Sends a message to logging tools
log(Message) ->
    werkzeug:logging("/home/andy/workspace/studium/semester5/vs"++
                     "/VS-2012-SKAW/"++
                     "NServer.log",
                     Message).


%% Processes incoming messages
%% Returns new dictionaries
drpMsg(Nachricht, Number, Delivery, Holdback, MaxDelivery) ->
    log("Dropmessage: " ++ integer_to_list(Number) ++ " --- " ++ now_to_list() ++ "\n"),
    dropMsg_(Nachricht, Number, Delivery, Holdback, MaxDelivery, maxKey(Delivery)).
%% Helper function
dropMsg_(Nachricht,Number, Delivery, Holdback, _MaxDelivery, {not_ok,_MayKey}) ->
    NewDelivery = dict:append(Number,Nachricht,Delivery),
    {NewDelivery, Holdback};
dropMsg_(Nachricht, Number, Delivery, Holdback, MaxDelivery, {ok,MaxKey}) ->
    NewNumber = MaxKey + 1,
    if Number =:= NewNumber ->
        {NewDelivery,NewHoldback} = checkHoldback(dict:append(Number,
                                                              Nachricht,
                                                              Delivery),
                                                  Holdback),
        Delivery2 = trimDelivery(NewDelivery, MaxDelivery),
        {Delivery2,NewHoldback};

       Number > NewNumber ->
            NewDelivery = Delivery,
            NewHoldback = dict:append(Number,Nachricht,Holdback),
            checkHoldbackGaps(NewDelivery, NewHoldback, MaxDelivery)
    end.


%% Finding maximum key of a dictonary
%% Returns tuple of {ok,maxKey} of a dict when dictionary is not empty
%% Returns tuple of {not_ok, -2} when dict is empty
maxKey(Delivery) ->
    maxKey_(Delivery, dict:size(Delivery)).
%% Helper function
maxKey_(Delivery, Size) when Size > 0 ->
    {ok,lists:max(dict:fetch_keys(Delivery))};
maxKey_(_, _) ->
    {not_ok, -2}.


%% Finding minimum key of a dictionary
%% Returns tuple of {ok,minKey} of a dictionary when dict is not empty
%% Returns tuple of {not_ok, -2} when dict is empty
minKey(Holdback) ->
    minKey_(Holdback, dict:size(Holdback)).
%% Helper function
minKey_(Holdback, Size) when Size > 0 ->
    {ok,lists:min(dict:fetch_keys(Holdback))};
minKey_(_,_) ->
    {not_ok, -2}.


%% Checks Holdbackqueue for messages which can be inserted in the sequence
%% of the Deliverqueue (bsp: Delivery = [{1,msg},{2,msg}]
%%                           Holdback = [{3,msg},{4,msg}]
%%                        -> Delivery = [{1,msg},{2,msg},{3,msg},{4,msg}]
%%                        -> Holdback = []
%% Returns modified dictionaries
checkHoldback(Delivery, Holdback) ->
    checkHoldback_(Delivery, Holdback, maxKey(Delivery), minKey(Holdback)).
%% Helper function
%% Checks for empty dictionaries
%% Returns modified dictionaries (if possible)
checkHoldback_(Delivery, Holdback, {ok,Max}, {ok,Min}) ->
    if Max+1 =:= Min ->
         NewDelivery = dict:append(Min,
                                   dict:fetch(Min,Holdback),
                                   Delivery),
         NewHoldback = dict:erase(Min, Holdback),
         checkHoldback(NewDelivery, NewHoldback);
    true ->
        {Delivery, Holdback}
    end;
%% BaseCase
checkHoldback_(Delivery, Holdback, _, _) ->
    {Delivery, Holdback}.


%% Checks for gaps in Holdback dictionary when Holdbacksize > MaxDeliverySize/2
%% and fills them with "Fehlende Nachricht"
%% (bsp: Delivery = [{1,msg},{2,msg},{3,msg},{4,msg},{5,msg}]
%%       Holdback = [{7,msg},{8,msg},{10,msg}]
%%    -> Delivery = [{1,msg},{2,msg},{3,msg},{4,msg},{5,msg},
%%                   {6, "Fehlende Nachricht"},{7,msg},{8,msg}]
%%    -> Holdback = [{10,msg}]
%% Returns modified dictionaries
checkHoldbackGaps(Delivery, Holdback, MaxSize) -> 
    checkHoldbackGaps_(Delivery,
                       Holdback,
                       MaxSize/2,
                       dict:size(Holdback),
                       MaxSize).
%% Helper function
checkHoldbackGaps_(Delivery,
                   Holdback,
                   MaxSize,
                   HoldbackSize,
                   OriginalMaxSize) when HoldbackSize > MaxSize ->
    {_,Key} = maxKey(Delivery),
    NewDelivery = dict:append(Key+1, fehlernachricht(), Delivery),
    {NewDel, NewHB} = checkHoldback(NewDelivery, Holdback),
    checkHoldbackGaps(NewDel, NewHB, OriginalMaxSize);
%% BaseCase
checkHoldbackGaps_(Delivery, Holdback,_,_,_) ->
    {Delivery, Holdback}.


%% Creates a filler for missing messages
%% Returns "Fehlende Nachricht"
fehlernachricht() ->
    "Fehlende Nachricht".


%% Checks whether Delivery is greater than MaxDelivery and removes
%% old messages until size of Delivery is equal to MaxDelivery
%% Returns trimmed delivery
trimDelivery(Delivery,MaxDelivery) ->
    trimDelivery_(Delivery, dict:size(Delivery), MaxDelivery, minKey(Delivery)).
%% Helper function
trimDelivery_(Delivery, Size, MaxSize, {ok, MinKey}) when Size > MaxSize ->
    trimDelivery(dict:erase(MinKey,Delivery), MaxSize);
%% BaseCase
trimDelivery_(Delivery,_,_,_) ->
    Delivery.


%% Checks which messages fit a client
getMessages(CPid,VPid,Delivery) ->
    VPid ! {getNo, CPid, self()},
    receive
        {ClientPid, error} ->
            io:format("client nicht gefunden~n"),
            {_, Min} = minKey(Delivery),
            sendMsg(ClientPid,Delivery,oldest),
            VPid ! {storeNo, ClientPid, Min+1};
        {ClientPid, {ok, Value}} ->
            io:format("client gefunden, Value: ~p~n",[Value]),
            sendMsg(ClientPid, Delivery, Value),
            VPid ! {storeNo, ClientPid, (Value+1)}
    end.


%% Sends messages to a client
sendMsg(CPid, Delivery, oldest) ->
    {_, Min} = minKey(Delivery),
    sendMsg_(CPid, dict:find(Min,Delivery),Delivery, Min);
sendMsg(CPid, Delivery, Number) ->
    sendMsg_(CPid, dict:find(Number, Delivery), Delivery, Number).
%% Helper function
sendMsg_(CPid, {ok, Msg}, Delivery, Number) ->
    More = hasMoreMsgs(Delivery, Number),
    if More ->
        CPid ! {Msg, false};
    true ->
        CPid ! {Msg, true}
    end;
sendMsg_(CPid, error, _, _) ->
    CPid ! {"Nachricht nichtmehr verfuegbar\n", false}.


%% Checks for more messages fitting a client
hasMoreMsgs(Delivery, Number) ->
    not ({ok,Number} =:= maxKey(Delivery)).

