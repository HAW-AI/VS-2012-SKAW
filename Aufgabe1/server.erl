-module(server).
-export([start/1]).
-author("Sebastian Krome, Andreas Wimmer").


%% public start function
start(Name) ->
    Pid = spawn(fun() -> loop(0, dict:new(), dict:new()) end),
    register(Name, Pid).


%% Main loop waiting for messages
loop(MsgNumber, Delivery, Holdback) ->
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
                                                Holdback),
            loop(MsgNumber,NewDelivery,NewHoldback)
    end,
    loop(MsgNumber+1,Delivery,Holdback).


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
drpMsg(Nachricht, Number, Delivery, Holdback) ->
    AktNumber = maxKey(Delivery)+1,
    if AktNumber =:= Number ->
        {NewDelivery,NewHoldback} = checkHoldback(dict:append(Number,
                                                              Nachricht,
                                                              Delivery),
                                                  Holdback),
        {NewDelivery,NewHoldback};
        true ->
            NewDelivery = Delivery,
            NewHoldback = dict:append(Number,Nachricht,Holdback),
            {NewDelivery, NewHoldback}
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


