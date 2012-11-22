-module(ggtProcess).
-export([start/2]).
-authors("sebastian krome, andreas wimmer").
-record(configVals, {praktikumsgruppe,
					teamno,
					koordinatoradress,
					nameserviceadress,
                    starterno}).


start({ArbeitsZeit, TermZeit, GGTProzessNummer}, ConfigRecord) ->
    spawn(fun() -> start_({ArbeitsZeit, TermZeit, GGTProzessNummer}, ConfigRecord) end).

%%TODO Arbeitszeit und Termzeit integrieren
start_({Arbeitszeit, Termzeit, GGTProzessNummer}, ConfigRecord) ->
    {processName, ProcessName} = createProcessName(GGTProzessNummer, ConfigRecord),
    register(ProcessName, self()),

    ConfigRecord#configVals.nameserviceadress ! {self(),
                                                {rebind,
                                                 ProcessName,
                                                 node()}},

    receive
        ok -> log(atom_to_list(ProcessName) ++ ": bound\n")
    end,

    ConfigRecord#configVals.koordinatoradress ! {hello,
                                                 ProcessName},
    loop(Arbeitszeit, ProcessName, ConfigRecord, Termzeit).

%% waiting for neighbors - loop
loop(Arbeitszeit, ProcessName, ConfigRecord, Termzeit) ->
    receive
        kill ->
            unbind(ProcessName, ConfigRecord),
            log(atom_to_list(ProcessName) ++ ": byebye");
        {setneighbors, N1, N2} ->
              if is_atom(N1),is_atom(N2) ->
                    log("got Neighbors: "
                        ++atom_to_list(N1)
                        ++" "++atom_to_list(N2)++"\n");
              true ->
                    log("got not atom Neighbors\n")
              end,
             ConfigRecord#configVals.nameserviceadress ! {self(), {lookup, N1}},
                 receive
                    undefined -> 1;
                    N11 -> ConfigRecord#configVals.nameserviceadress ! {self(), {lookup, N2}},
                        receive
                            undefined -> 1;
                            N22 -> loop(Arbeitszeit, ProcessName, N11, N22, ConfigRecord, Termzeit, 0)
                        end
                end;

        {tellmi,From} ->
            From ! -1;
        _ -> log("received nothing useful\n"),
             loop(Arbeitszeit, ProcessName, ConfigRecord, Termzeit)
    end.

%% waiting for mi loop
loop(Arbeitszeit, ProcessName, N1, N2, ConfigRecord, Termzeit, TRef) ->
  receive
    {setpm,MiNeu} ->
        Refs = checkTimer(TRef, Termzeit),
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(computing, Arbeitszeit, ProcessName, N1, N2, MiNeu, ConfigRecord, Termzeit, Refs);
    {setneighbors, _, _} ->
        loop(Arbeitszeit, ProcessName, N1, N2, ConfigRecord, Termzeit, TRef);
    {tellmi,From} ->
            From ! -1;
    kill ->
        unbind(ProcessName, ConfigRecord),
        werkzeug:logstop()
  end.

%% berechnungs-loop
loop(State, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef) ->
  receive
    {sendy, Y} when Y < Mi ->
        timer:sleep(Arbeitszeit*1000),
        Refs = checkTimer(TRef, Termzeit),
        NewMi = ((Mi-1) rem Y)+1,
        log("Received "++integer_to_list(Y)++"; neues Mi: "++integer_to_list(NewMi)++"\n"),
        N1 ! {sendy,NewMi},
        N2 ! {sendy,NewMi},
        ConfigRecord#configVals.koordinatoradress ! {briefmi, {ProcessName,NewMi,erlang:time()}},
        loop(computing, Arbeitszeit, ProcessName, N1, N2, NewMi, ConfigRecord, Termzeit, Refs);
    {sendy, Y} ->
        Refs = checkTimer(TRef, Termzeit),
        log("Received "++integer_to_list(Y)++"behalte Mi: "++integer_to_list(Mi)++"\n"),
        loop(computing, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, Refs);
    {setneighbors, _, _} ->
        loop(State, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {setpm,MiNeu} -> 
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(State, Arbeitszeit, ProcessName, N1, N2, MiNeu, ConfigRecord, Termzeit, TRef);
    {tellmi,From} ->
            From ! Mi,
            loop(State, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    half ->
        loop(half, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    complete ->
        N2 ! {abstimmung, ProcessName},
        loop(complete, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {abstimmung, ProcessName} ->
            log("Eigene Abstimmung erhalten -> Ring komplett durchlaufen\n"),
            case State of
                    complete -> log("State: Complete, send briefterm\n"),
                                ConfigRecord#configVals.koordinatoradress ! {briefterm,
                                                                         {ProcessName, Mi, erlang:time()}};
		    _ -> log("State: not complete.\n")
            end,
        loop(State, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {abstimmung, Initiator} ->
	    if
		 is_atom(Initiator) -> log("Abstimmung erhalten von "++atom_to_list(Initiator)++"\n");
		 is_pid(Initiator) -> log("Abstimmung erhalten von "++pid_to_list(Initiator)++"\n");
		 true -> log("Abstimmung erhalten\n")
	    end,

            log("State: "++ atom_to_list(State) ++ "\n"),
        case State of
                computing -> loop(computing, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
                half -> N2 ! {abstimmung, Initiator},
                        loop(half, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
                complete -> N2 ! {abstimmung, Initiator},
                        loop(complete, Arbeitszeit, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef)
        end;



    kill -> unbind(ProcessName, ConfigRecord),
            werkzeug:logstop()
  end.


unbind(ProcessName, ConfigVals) ->
    ConfigVals#configVals.nameserviceadress ! {self(), {unbind, ProcessName}},
    receive
        ok -> log(atom_to_list(ProcessName) ++ ": unbind"
              ++ "bye bye\n")
    end.


createProcessName(ProcessNo, ConfigRecord) ->
    ProcessName = integer_to_list(ConfigRecord#configVals.praktikumsgruppe)
                  ++integer_to_list(ConfigRecord#configVals.teamno)
                  ++integer_to_list(ProcessNo)
                  ++integer_to_list(ConfigRecord#configVals.starterno),
    {processName, list_to_atom(ProcessName)}.


log(Message) ->
	Endung = "GGTProcess: "++pid_to_list(self()),
	tools:log(Message,Endung).


checkTimer(0, Termzeit) ->
    {ok, HalfRef} = timer:send_after(Termzeit*500 ,half),
    {ok, ComplRef} = timer:send_after(Termzeit*1000, complete),
    {refs, HalfRef, ComplRef};
checkTimer({refs, HR, CR}, Termzeit) ->
    timer:cancel(HR),
    timer:cancel(CR),
    checkTimer(0, Termzeit).















