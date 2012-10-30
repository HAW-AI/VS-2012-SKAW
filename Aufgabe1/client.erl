-module(client).
-export([start/2,change/1]).
-author("Sebastian Krome, Andreas Wimmer").


%% public start function
start(Pid, Name) ->
    MyPid = spawn(fun() -> editor_loop(5000, Pid, 0) end),
    register(Name, MyPid).


%% Editor loop waiting for incoming messages
%% Used when client is in editor state
editor_loop(Timeout, Pid, 5) -> read_loop(Timeout, Pid);
editor_loop(Timeout, Pid, MessageNo) ->
    erlang:start_timer(Timeout, self(), sendTimeout),
    receive
        {timeout,_,sendTimeout} ->
            Pid ! {getmsgid, self()},
            receive
                Number -> Pid ! {newMsg(Number), self()},
                log(newMsg(Number))
            end;
        Any -> io:format("received: " ++ Any)
    end,
    editor_loop(Timeout, Pid, MessageNo +1).


%% Read loop waiting for incoming messages
%% Used when client is in read state
read_loop(Timeout, Pid) ->
    Pid ! {getmessages, self()},
    receive
        {Nachrichteninhalt, true} -> gotLastMessage(Nachrichteninhalt,
                                                    Timeout,
                                                    Pid);
                                                    %% keine weiteren Nachrichten
        {Nachrichteninhalt, false} -> gotMessage(Nachrichteninhalt,
                                                 Timeout,
                                                 Pid)
                                                 %% weitere Nachrichten
    end.


%% Creates the new message to be sent and logged
%% Returns message (bsp: "lab2233 MsgNumber: 13 --- Time: 15:28:00")
newMsg(Number) ->
    "lab2233 MsgNumber: "
    ++integer_to_list(Number)
    ++" --- Time: "
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
                     "client_3lab22.log",
                     Message).


%% Logs a message
%% Changes the clients state to editor loop with new timeout time
gotLastMessage(Message,Timeout,Pid) ->
    log("Got last Message: "++Message),
    editor_loop(change(Timeout), Pid, 0).


%% Logs a message
%% Does not change the clients state
gotMessage(Message,Timeout, Pid) ->
    log("Got Message: "++Message),
    read_loop(Timeout, Pid).


%% Changes the timeout time given to loops
%% Returns new timeout time
change(Time) ->
    Faktor = case random:uniform(2) of
                    1 -> -0.5;
                    _ -> 0.5
    end,
    NewTime = Time + (Time * Faktor),

    case NewTime < 1 of
        true -> 1;
        _ -> round(NewTime)
    end.



