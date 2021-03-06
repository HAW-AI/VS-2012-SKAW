-module(koordinator).
-export([start/0, neighbours/3,computeMis/3,miSingleStep/2]).
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
            loop(bereit, Nameservice, SteeringVals, ProcessList);

        kill ->
            killGGT(ProcessList, Nameservice),
            log("Bye Bye\n"),
            werkzeug:logstop(),
            Nameservice ! {self(), {unbind, SteeringVals#steeringVals.koordinatorname}},
            receive
                ok -> log("koordinator unbind\n")
            end,
            exit(normal)

	end;

loop(bereit, Nameservice, SteeringVals, ProcessList) ->
    receive
        {setMi, GGT} -> 
            log("received setMi: "++integer_to_list(GGT)),
            Mis = computeMis(ProcessList,GGT,[]),
            distributeMis(ProcessList,Mis,Nameservice),
            FifteenPList = getFifteenPercent(ProcessList),
            %distributedStart(FifteenPList, Nameservice, GGT),
            distributedStart(FifteenPList, Nameservice, computeMi(GGT)),
            loop(bereit, Nameservice, SteeringVals, ProcessList);
        reset ->
            log("killing all GGT Processes\n"),
            killGGT(ProcessList, Nameservice),
            loop(init,Nameservice,SteeringVals,[]);
        {briefmi, {Clientname, CMi, _CZeit}} ->
            log("received: briefmi from "
                ++atom_to_list(Clientname)
                ++" with Mi: "
                ++integer_to_list(CMi)
                ++"\n"),
            loop(bereit, Nameservice, SteeringVals, ProcessList);
        {briefterm, {_Clientname, CMi, _CZeit}} ->
            log("Calculation exited: "
                ++ " RESULT: "
                ++integer_to_list(CMi)
                ++"\n"),
            loop(bereit, Nameservice, SteeringVals, ProcessList);
        kill ->
            killGGT(ProcessList, Nameservice),
            log("Bye Bye\n"),
            werkzeug:logstop(),
            Nameservice ! {self(), {unbind, SteeringVals#steeringVals.koordinatorname}},
            receive
                ok -> log("koordinator unbind\n")
            end,
            exit(normal)
    end.


killGGT([], _) ->
    1;
killGGT([H|T], Nameservice) ->
    Nameservice ! {self(), {lookup, H}},
    receive
        not_found ->
            log(atom_to_list(H)
                ++ " not found while building ring\n");
        {Name, Node} ->
            log(atom_to_list(Name)
                ++ "kill gesendet\n"),
            {Name, Node} ! kill
    end,
    killGGT(T, Nameservice).


computeMis([],_GGT,AccList) ->
    AccList;
computeMis([_H|T],GGT,AccList)->
    random:seed(),
    random:seed(now()),
    Mi = computeMi(GGT),
    computeMis(T,GGT,AccList++[Mi]).


computeMi(GGT)->
    Prims = [3,5,11,13,23,37],
    F = fun(Prim,Acc) -> miSingleStep(Prim,Acc)end,
    GGT*lists:foldl(F,1,Prims).


miSingleStep(Prim,Acc)->
    case random:uniform(3) of
        1 -> Acc;
        2 -> Acc*Prim;
        3 -> trunc(Acc*math:pow(Prim,2))
    end.


distributeMis([],_,_) -> 1;
distributeMis([GGTProcess|Tail],[Mi|MiTail],Nameservice)->
    Nameservice ! {self(), {lookup, GGTProcess}},
    receive
        not_found ->
            log(atom_to_list(GGTProcess)
                ++ " not found while distributing Mis\n");
        {Name, Node} ->
            log(atom_to_list(Name)
                ++ " setze Mi: "
                ++ integer_to_list(Mi)
                ++ "\n"),
            {Name, Node} ! {setpm, Mi}
    end,
    distributeMis(Tail,MiTail,Nameservice).


buildProcessRing([],_) ->
    [];
buildProcessRing(ProcessList, Nameservice) ->
    NewProcessList = werkzeug:shuffle(ProcessList),
    buildProcessRing_(1, listSize(NewProcessList), NewProcessList, Nameservice).


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


listSize(List) ->
    listSize_(List, 0).


listSize_([], Acc) ->
    Acc;
listSize_([_|T], Acc) ->
    listSize_(T, Acc+1).


log(Message) ->
	Endung = "Koordinator",
	tools:log(Message,Endung).


getFifteenPercent(List) ->
    FifteenPercent = trunc(listSize(List) * 0.15)+1,
    werkzeug:shuffle(List),
    lists:sublist(List, FifteenPercent).


distributedStart([], _, _) ->
    1;
distributedStart([H|T], Nameservice, Number) ->
    Nameservice ! {self(), {lookup, H}},
    receive
        not_found ->
            log(atom_to_list(H)
                ++ " not found while starting calculation\n");
        {briefmi, {Clientname, CMi, _CZeit}} ->
            log(atom_to_list(Clientname)
                ++ " got new mi "
                ++ integer_to_list(CMi)
                ++ "\n");
        {Name, Node} ->
            log(atom_to_list(Name)
                ++ " start calculation "
                ++ "\n"),
            {Name, Node} ! {sendy, Number}
    end,

    distributedStart(T, Nameservice, Number).


