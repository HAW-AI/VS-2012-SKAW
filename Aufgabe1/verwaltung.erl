-module(verwaltung).
-export([start/1]).


start(ForgetAfter) ->
	Pid = spawn(fun() -> loop(ForgetAfter,dict:new(),dict:new()) end),
	Pid.


loop(ForgetAfter ,Dict, TimerDict) ->
	receive
		{getNo, KeyPid, Sender} -> getNo(Dict,KeyPid, Sender),
			loop(ForgetAfter, Dict, TimerDict);
		{storeNo, KeyPid, No} -> {NewDict,NewTimerDict} = storeNo(KeyPid,No, Dict, TimerDict, ForgetAfter),
			loop(ForgetAfter, NewDict, NewTimerDict);
		{timeout,KeyPid} -> {NewDict, NewTimerDict} = delete(Dict, TimerDict, KeyPid),
			loop(ForgetAfter, NewDict, NewTimerDict);
		kill -> io:format("Client verwaltung tot\n")
	end.

getNo(Dict,Pid, Sender) ->
	Value = dict:find(Pid,Dict),
	Sender ! {Pid,Value}.

storeNo(Pid, No, Dict, TimerDict, ForgetAfter) ->
	NewDict = dict:store(Pid,No,Dict),
	killTimer(TimerDict,Pid),
	{_,TimerPid} = timer:send_after(ForgetAfter, {timeout,Pid}),
	NewTimerDict = updateTimer(TimerDict,TimerPid, Pid),
	{NewDict,NewTimerDict}.

killTimer(TimerDict,KeyPid) ->
	killTimer_(dict:find(KeyPid,TimerDict)).

killTimer_({ok,Value}) ->
	timer:cancel(Value);
killTimer_(_) ->
	io:format("Konnte Timer nicht finden\n").

updateTimer(TimerDict, TimerPid, KeyPid) ->
	NewTimerDict = dict:store(KeyPid,TimerPid, TimerDict),
	NewTimerDict.

delete(Dict, TimerDict, Pid) ->
	io:format("loesche: "++pid_to_list(Pid)++"\n"),
	NewDict = dict:erase(Pid,Dict),
	NewTimerDict = dict:erase(Pid,TimerDict),
	{NewDict, NewTimerDict}.


