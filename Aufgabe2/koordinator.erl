-module(koordinator).
-export([start/0, neighbours/3]).
-authors("sebastian krome, andreas wimmer").
-record(steeringVals, {arbeitszeit, 
						termzeit,
						ggtProzessnummer,
						nameservicenode,
						koordinatorname}).

start() ->
    spawn(fun() -> start_() end).

start_() ->
	{Arbeitszeit,
     Termzeit,
     Ggtprozessnummer,
     Nameservicenode,
     Koordinatorname} = tools:getKoordinatorConfigData(),

	register(Koordinatorname,self()),
	SteeringVals = #steeringVals{arbeitszeit = Arbeitszeit,
								termzeit = Termzeit,
								ggtProzessnummer = Ggtprozessnummer,
								nameservicenode = Nameservicenode,
								koordinatorname = Koordinatorname},
	net_adm:ping(Nameservicenode),
	global:sync(),
	Nameservice = global:whereis_name(nameservice),
	Nameservice ! {self(), {rebind, Koordinatorname, node()}},
    loop(init, Nameservice, SteeringVals, []).


loop(init, Nameservice, SteeringVals, ProcessList) ->
	receive 
		{getsteeringval, Pid} ->
            Pid ! {steeringval,
		     	   SteeringVals#steeringVals.arbeitszeit,
				   SteeringVals#steeringVals.termzeit,
				   SteeringVals#steeringVals.ggtProzessnummer},
            loop(init, Nameservice, SteeringVals, ProcessList);

        {hello, ProcessName} ->
            if is_atom(ProcessName) ->
                log("Client: "
                     ++ atom_to_list(ProcessName)
                     ++ " angemeldet\n"),

                loop(init, Nameservice, SteeringVals, ProcessList ++ [ProcessName]);
            true ->
                log("Got illegal ProcessNameType\n")
            end,
            loop(init, Nameservice, SteeringVals, ProcessList);

        ready ->
            log("received ready\n"),
            buildProcessRing(ProcessList, Nameservice),
            loop(bereit, Nameservice, SteeringVals, ProcessList)


	end;

loop(bereit, _Nameservice, _SteeringVals, _ProcessList) ->
    1.

buildProcessRing([],_) ->
    [];
buildProcessRing(ProcessList, Nameservice) ->
    NewProcessList = werkzeug:shuffle(ProcessList),
    buildProcessRing_(1, listSize(NewProcessList, 0), NewProcessList, Nameservice).


buildProcessRing_(Count, Length, _, _) when Length < Count ->
    log("BuildRing: ready!!\n");
buildProcessRing_(Count, Length, List, Nameservice) ->
    CurrentProcess = lists:nth(Count, List),
    {N1, N2} = neighbours(Count, Length, List),
    Nameservice ! {self(), {lookup, CurrentProcess}},
    receive
        not_found ->
            log(atom_to_list(CurrentProcess)
                ++ " not found while building ring\n");
        {Name, Node} ->
            log(atom_to_list(Name)
                ++ " setze Nachbarn"
                ++ " Links: " ++ atom_to_list(N1)
                ++ " Rechts: " ++ atom_to_list(N2)
                ++ "\n"),
            {Name, Node} ! {setneighbors, N1, N2}
    end,
    buildProcessRing_(Count+1, Length, List, Nameservice).


neighbours(_,1,_) -> error;
neighbours(_,0,_) -> error;
neighbours(1,Length, L) ->
    {lists:nth(Length, L),lists:nth(2, L)};
neighbours(Length, Length, L) ->
    {lists:nth(Length-1, L),lists:nth(1, L)};
neighbours(X, _Length, L) ->
    {lists:nth(X-1, L),lists:nth(X+1, L)}.


listSize([], Acc) ->
    Acc;
listSize([_|T], Acc) ->
    listSize(T, Acc+1).

log(Message) ->
    io:format(Message),
	Endung = "Koordinator",
	tools:log(Message,Endung).











