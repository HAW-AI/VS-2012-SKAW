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
        ok -> log(ProcessName ++ ": bound")
    end,

    ConfigRecord#configVals.koordinatoradress ! {hello,
                                                 list_to_atom(ProcessName)},
    loop(ProcessName).

loop(ProcessName) ->
    receive
        kill -> log(ProcessName ++ ": byebye");
        {setneighbors, N1, N2} -> 
              if is_atom(N1),is_atom(N2) -> log("got Neighbors: "++atom_to_list(N1)
                                                  ++" "++atom_to_list(N2)++"\n");
                 true -> log("got not atom Neighbors")
              end,
              loop(ProcessName, N1, N2);
        _ -> log("received nothing useful"),
             loop(ProcessName)
    end.

loop(ProcessName, N1,N2) -> 
  receive
    {setpm,MiNeu} -> 
        log("Got new pm: "++integer_to_list(MiNeu)),
        loop(ProcessName, N1,N2,Mi);
    kill -> log(ProcessName ++ ": byebye")
  end.

loop(ProcessName, N1, N2, Mi) ->
  1.




createProcessName(ProcessNo, ConfigRecord) ->
    ProcessName = integer_to_list(ConfigRecord#configVals.praktikumsgruppe)
                  ++integer_to_list(ConfigRecord#configVals.teamno)
                  ++integer_to_list(ProcessNo)
                  ++integer_to_list(ConfigRecord#configVals.starterno),
    {processName, ProcessName}.

log(Message) ->
	Endung = "GGTProcess: "++pid_to_list(self()),
	tools:log(Message,Endung).


















