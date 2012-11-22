-module(starter).
-export([start/1]).
-authors("sebastian krome, andreas wimmer").
-record(configVals, {praktikumsgruppe, 
					teamno,
					koordinatoradress,
					nameserviceadress,
                    starterno}).

start(Number) ->
    spawn(fun() -> start_(Number) end).

start_(Number) ->
	 {Praktikumsgruppe,
      Teamnummer,
      Nameservicenode,
      Koordinatorname} = tools:getGgtConfigData(),

	 net_adm:ping(Nameservicenode),
	 global:sync(),
	 Nameservice = global:whereis_name(nameservice),

	 %% Koordinatoradresse beim Namensdienst erfragen
	 Nameservice ! {self(),{lookup, Koordinatorname}},

	 receive
	 	not_found ->
            log("Koordinator "++Koordinatorname++" not found\n");

	 	{Koordinatorname, Node} ->
            log("Koordinator "++atom_to_list(Koordinatorname)++" found\n"),
	 		Config = #configVals{praktikumsgruppe = Praktikumsgruppe,
	 		 					 teamno = Teamnummer,
	 							 koordinatoradress = {Koordinatorname, Node},
	 							 nameserviceadress = Nameservice,
                                 starterno = Number},
            SteeringValues = getSteeringVal({Koordinatorname, Node}),
            startProcesses(SteeringValues, Config)
	 end.


getSteeringVal(KoordinatorAdress) ->
    KoordinatorAdress ! {getsteeringval, self()},

    receive
        {steeringval,
         ArbeitsZeit,
         TermZeit,
         GGTProzessnummer} -> {ArbeitsZeit, TermZeit, GGTProzessnummer}
    end.

startProcesses({ArbeitsZeit, TermZeit, 1}, ConfigRecord) ->
    ggtProcess:start({ArbeitsZeit, TermZeit, 1}, ConfigRecord);
%    spawn(ggtProcess, start, [{ArbeitsZeit, TermZeit, 1}, ConfigRecord]);
startProcesses({ArbeitsZeit, TermZeit, GGTProzessnummer}, ConfigRecord) ->
    ggtProcess:start({ArbeitsZeit, TermZeit, GGTProzessnummer}, ConfigRecord),
%    spawn(ggtProcess, start, [{ArbeitsZeit, TermZeit, GGTProzessnummer}, ConfigRecord]),
    startProcesses({ArbeitsZeit, TermZeit, GGTProzessnummer - 1}, ConfigRecord).

log(Message) ->
	Endung = "Starter"++pid_to_list(self()),
	tools:log(Message,Endung),
    werkzeug:logstop().



