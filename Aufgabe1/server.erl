-module(server).
-export([start/2,trimDelivery/2,minKey/1,checkHoldbackGaps/3]).
-author("Sebastian Krome, Andreas Wimmer").


%% public start function
start(Name,MaxDelivery) ->
    Pid = spawn(fun() -> loop(0, dict:new(), dict:new(),MaxDelivery) end),
    register(Name, Pid).


%% Main loop waiting for messages
loop(MsgNumber, Delivery, Holdback,MaxDelivery) ->
    receive
        {getmsgid, Pid} ->
            werkzeug:logging(
                "/home/andy/workspace/studium/semester5/vs"++
                "/VS-2012-SKAW/NServer_3lab22.log",
                serverLog(MsgNumber, number, Pid)),
            Pid ! MsgNumber;

        {dropmessage, {Nachricht, Number}} ->
            {NewDelivery, NewHoldback} = drpMsg(Nachricht,
                                                Number,
                                                Delivery,
                                                Holdback,
                                                MaxDelivery),
            loop(MsgNumber,NewDelivery,NewHoldback, MaxDelivery)
    end,
    loop(MsgNumber+1,Delivery,Holdback, MaxDelivery).


%% Logging function
%% Returns (bsp: "server: Nummer: 12 an <0.40.0> gesendet")
serverLog(MsgNumber, number, Pid) ->
    "server: Nummer: "
    ++integer_to_list(MsgNumber)
    ++" an "
    ++pid_to_list(Pid)
    ++" gesendet\n".


%% Processes incoming messages
%% Returns new dictionaries
drpMsg(Nachricht, Number, Delivery, Holdback, MaxDelivery) ->
    AktNumber = maxKey(Delivery)+1,
    if AktNumber =:= Number ->
        {NewDelivery,NewHoldback} = checkHoldback(dict:append(Number,
                                                              Nachricht,
                                                              Delivery),
                                                  Holdback),
        Delivery2 = trimDelivery(NewDelivery, MaxDelivery),
        {Delivery2,NewHoldback};
        true ->
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
%%                           Holdback = []
%% Returns modified dictionaries
checkHoldback(Delivery, Holdback) ->
    checkHoldback_(Delivery, Holdback, maxKey(Delivery), minKey(Holdback)).
    


%% Checks whether Delivery is greater than MaxDelivery and removes old messages until size of Delivery is equal to
%% MaxDelivery
%% returns trimmed delivery
trimDelivery(Delivery,MaxDelivery) ->
    trimDelivery_(Delivery, dict:size(Delivery), MaxDelivery).

trimDelivery_(Delivery, Size, MaxSize) when Size > MaxSize ->
    {_,MinKey} = minKey(Delivery),
    if MinKey > -2 ->
        trimDelivery(dict:erase(MinKey,Delivery), MaxSize)
    end;

trimDelivery_(Delivery, _,_) ->
    Delivery.

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
%% Returns unmodified dictionaries due to empty input dictionaries
checkHoldback_(Delivery, Holdback, _, _) ->
    {Delivery, Holdback}.

checkHoldbackGaps(Delivery, Holdback, MaxSize) -> 
    checkHoldbackGaps_(Delivery, Holdback, MaxSize/2, dict:size(Holdback),MaxSize).

checkHoldbackGaps_(Delivery, Holdback, MaxSize, HoldbackSize, OriginalMaxSize) when HoldbackSize > MaxSize ->
    {_,Key} = maxKey(Delivery),
    NewDelivery = dict:append(Key+1, fehlernachricht(), Delivery),
    {NewDel, NewHB} = checkHoldback(NewDelivery, Holdback),
    checkHoldbackGaps(NewDel, NewHB, OriginalMaxSize);

checkHoldbackGaps_(Delivery, Holdback,_,_,_) ->
    {Delivery, Holdback}.

fehlernachricht() ->
    "Fehlende Nachricht".


