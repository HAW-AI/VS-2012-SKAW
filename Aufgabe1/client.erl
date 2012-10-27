-module(client).
-export([start/2]).


%% public start function
start(Pid, Name) ->
    MyPid = spawn(fun() -> loop(5000, Pid) end),
    register(Name, MyPid).


%% Main loop waiting for incoming messages
loop(Timeout, Pid) ->
    erlang:start_timer(Timeout, self(), sendTimeout),
    receive
        {timeout,_,sendTimeout} ->
            Pid ! {getmsgid, self()},
            receive
                Number -> Pid ! {newMsg(Number), self()},
                werkzeug:logging(
                        "/home/andy/workspace/studium/semester5/vs"++
                        "/VS-2012-SKAW/"++
                        "client_3lab22.log",
                        newMsg(Number))
            end;
        Any -> io:format("received: " ++ Any)
    end,
    loop(Timeout, Pid).


%% Creates the new message to be sent and logged
%% Returns message (bsp: "lab2233 MsgNumber: 13 --- Time: 15:28:00")
newMsg(Number) ->
    {_,{Hour, Minutes, Seconds}} = erlang:localtime(),
    "lab2233 MsgNumber: "
    ++integer_to_list(Number)
    ++" --- Time: "
    ++integer_to_list(Hour)
    ++":"
    ++integer_to_list(Minutes)
    ++":"
    ++integer_to_list(Seconds)
    ++"\n".

