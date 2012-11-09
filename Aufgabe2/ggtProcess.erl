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
        _ -> log("received nothing useful"),
             loop(ProcessName)
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


















