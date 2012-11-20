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
start_({_Arbeitszeit, Termzeit, GGTProzessNummer}, ConfigRecord) ->
    {processName, ProcessName} = createProcessName(GGTProzessNummer, ConfigRecord),
    register(list_to_atom(ProcessName), self()),

    ConfigRecord#configVals.nameserviceadress ! {self(),
                                                {rebind,
                                                 list_to_atom(ProcessName),
                                                 node()}},

    receive
        ok -> log(ProcessName ++ ": bound\n")
    end,

    ConfigRecord#configVals.koordinatoradress ! {hello,
                                                 list_to_atom(ProcessName)},
    loop(ProcessName, ConfigRecord, Termzeit).

%% waiting for neighbors - loop
loop(ProcessName, ConfigRecord, Termzeit) ->
    receive
        kill -> log(ProcessName ++ ": byebye");
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
                            N22 -> loop(ProcessName, N11, N22, ConfigRecord, Termzeit, 0)
                        end
                end;

        {tellmi,From} ->
            From ! -1;
        _ -> log("received nothing useful\n"),
             loop(ProcessName, ConfigRecord, Termzeit)
    end.

%% waiting for mi loop
loop(ProcessName, N1, N2, ConfigRecord, Termzeit, TRef) ->
  receive
    {setpm,MiNeu} ->
        Refs = checkTimer(TRef, Termzeit),
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(computing, ProcessName, N1, N2, MiNeu, ConfigRecord, Termzeit, Refs);
    {setneighbors, _, _} ->
        loop(ProcessName, N1, N2, ConfigRecord, Termzeit, TRef);
    {tellmi,From} ->
            From ! -1;
    kill -> log(ProcessName ++ ": byebye\n")
  end.

%% berechnungs-loop
loop(State, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef) ->
  receive
    {sendy, Y} when Y < Mi ->
        Refs = checkTimer(TRef, Termzeit),
        NewMi = ((Mi-1) rem Y)+1,
        log("Received "++integer_to_list(Y)++"; neues Mi: "++integer_to_list(NewMi)++"\n"),
        N1 ! {sendy,NewMi},
        N2 ! {sendy,NewMi},
        ConfigRecord#configVals.koordinatoradress ! {briefmi, {list_to_atom(ProcessName),NewMi,erlang:time()}},
        loop(computing, ProcessName, N1, N2, NewMi, ConfigRecord, Termzeit, Refs);
    {sendy, Y} ->
        Refs = checkTimer(TRef, Termzeit),
        log("Received "++integer_to_list(Y)++"behalte Mi: "++integer_to_list(Mi)++"\n"),
        loop(computing, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, Refs);
    {setneighbors, _, _} ->
        loop(State, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {setpm,MiNeu} -> 
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(State, ProcessName, N1, N2, MiNeu, ConfigRecord, Termzeit, TRef);
    {tellmi,From} ->
            From ! Mi,
            loop(State, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    half ->
        loop(half, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    complete ->
        N2 ! {abstimmung, ProcessName},
        loop(complete, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {abstimmung, ProcessName} ->
            log("Abstimmung erhalten\n"),
            case State of
                    complete -> log("State: Complete, send briefterm\n"),
                                ConfigRecord#configVals.koordinatoradress ! {briefterm,
                                                                         {list_to_atom(ProcessName), Mi, erlang:time()}}
            end,
        loop(State, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
    {abstimmung, Initiator} ->
            log("Abstimmung erhalten\n"),
            log("State: "++ atom_to_list(State) ++ "\n"),
        case State of
                computing -> loop(computing, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
                half -> N2 ! {abstimmung, Initiator},
                        loop(half, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef);
                complete -> N2 ! {abstimmung, Initiator},
                        loop(complete, ProcessName, N1, N2, Mi, ConfigRecord, Termzeit, TRef)
        end;



    kill -> log(ProcessName ++ ": byebye\n"),
            werkzeug:logstop()
  end.





createProcessName(ProcessNo, ConfigRecord) ->
    ProcessName = integer_to_list(ConfigRecord#configVals.praktikumsgruppe)
                  ++integer_to_list(ConfigRecord#configVals.teamno)
                  ++integer_to_list(ProcessNo)
                  ++integer_to_list(ConfigRecord#configVals.starterno),
    {processName, ProcessName}.

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















