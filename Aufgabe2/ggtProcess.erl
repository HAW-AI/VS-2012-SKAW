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
start_({_ArbeitsZeit, _TermZeit, GGTProzessNummer}, ConfigRecord) ->
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
    loop(ProcessName).

%% waiting for neighbors - loop
loop(ProcessName, ConfigRecord) ->
    receive
        kill -> log(ProcessName ++ ": byebye");
        {setneighbors, N1, N2} -> 
              if is_atom(N1),is_atom(N2) -> log("got Neighbors: "++atom_to_list(N1)
                                                  ++" "++atom_to_list(N2)++"\n");
                 true -> log("got not atom Neighbors\n")
              end,
              loop(ProcessName, N1, N2, ConfigRecord);
        {tellmi,From} ->
            From ! -1
        _ -> log("received nothing useful\n"),
             loop(ProcessName, ConfigRecord)
    end.

%% waiting for mi loop
loop(ProcessName, N1,N2,ConfigRecord) -> 
  receive
    {setpm,MiNeu} -> 
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(ProcessName, N1,N2,MiNeu,ConfigRecord);
    {setneighbors, New1,New2} ->
        loop(Processanme,New1,New2,ConfigRecord);
    {tellmi,From} ->
            From ! -1
    kill -> log(ProcessName ++ ": byebye\n")
  end.

%% berechnungs-loop
loop(ProcessName, N1, N2, Mi,ConfigRecord) ->
  receive 
    {sendy, Y} when Y < Mi ->
        NewMi = ((Mi-1) rem Mi)+1,
        log("Received "++integer_to_list(Y)++"; neues Mi: "++integer_to_list(NewMi)++"\n"),
        N1 ! {sendy,NewMi},
        N2 ! {sendy,NewMi},
        ConfigRecord#configVals.koordinatoradress ! {briefmi, {Processname,NewMi,erlang:time()}}
        loop(Processname,N1,N2,NewMi,ConfigRecord);
    {sendy, Y} -> 
        log("Received "++integer_to_list(Y)++"behalte Mi: "++integer_to_list(Mi)++"\n"),
        loop(Processname,N1,N2,Mi,ConfigRecord);
    {setneighbors, New1,New2} ->
        loop(Processanme,New1,New2,ConfigRecord);
    {setpm,MiNeu} -> 
        log("Got new pm: "++integer_to_list(MiNeu)++"\n"),
        loop(ProcessName, N1,N2,MiNeu,ConfigRecord);
    {tellmi,From} ->
            From ! Mi;
    kill -> log(ProcessName ++ ": byebye\n")
  end





createProcessName(ProcessNo, ConfigRecord) ->
    ProcessName = integer_to_list(ConfigRecord#configVals.praktikumsgruppe)
                  ++integer_to_list(ConfigRecord#configVals.teamno)
                  ++integer_to_list(ProcessNo)
                  ++integer_to_list(ConfigRecord#configVals.starterno),
    {processName, ProcessName}.

log(Message) ->
	Endung = "GGTProcess: "++pid_to_list(self()),
	tools:log(Message,Endung).


















