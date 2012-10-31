-module(client).
-export([start/3,start_n/1]).
-import(tools).
-author("Sebastian Krome, Andreas Wimmer").


start_n(Pid)->
    {Clients, Lifetime, Servername, Intervall} = tools:getClientConfigData(),
    start_n_(Pid, Lifetime*1000, Servername, Intervall*1000, Clients).
start_n_(Pid, Lifetime, Servername, Intervall, 1) ->
    ClientPid={Servername,Pid},
    start(ClientPid,Lifetime,Intervall);
start_n_(Pid, Lifetime, Servername, Intervall, Clients) ->
    ClientPid={Servername, Pid},
    start(ClientPid,Lifetime,Intervall),
    start_n_(Pid, Lifetime, Servername, Intervall, Clients-1).

%% public start function
start(Pid,Lifetime,Intervall) ->
    MyPid = spawn(fun() -> editor_loop(Intervall, Pid, 0) end),
    timer:send_after(Lifetime, MyPid, die).


%% Editor loop waiting for incoming messages
%% Used when client is in editor state
editor_loop(Timeout, Pid, 5) -> read_loop(Timeout, Pid);
editor_loop(Timeout, Pid, MessageNo) ->
    erlang:start_timer(Timeout, self(), sendTimeout),
    receive
        {timeout,_,sendTimeout} ->
            Pid ! {getmsgid, self()},
            receive
                Number -> Pid ! {dropmessage,{newMsg(Number), Number}},
                    log(newMsg(Number), pid_to_list(self()))
            end,
            editor_loop(Timeout, Pid, MessageNo + 1);
        die ->
            io:format("bye~n");
        Any -> io:format("received: " ++ Any++"~n")
    end.


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
log(Message,Endung) ->
    werkzeug:logging("/home/andy/workspace/studium/semester5/vs"++
                     "/VS-2012-SKAW/"++
                     "client_3lab22.log",
                     Message),

    werkzeug:logging("/home/andy/workspace/studium/semester5/vs"++
                     "/VS-2012-SKAW/"++
                     "client_3lab22"++
                     Endung++".log",
                     Message).


%% Logs a message
%% Changes the clients state to editor loop with new timeout time
gotLastMessage(Message,Timeout,Pid) ->
    log("Got last Message: "++Message++"\n", pid_to_list(self())),
    editor_loop(change(Timeout), Pid, 0).


%% Logs a message
%% Does not change the clients state
gotMessage(Message,Timeout, Pid) ->
    log("Got Message: "++Message++"~n", pid_to_list(self())),
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



