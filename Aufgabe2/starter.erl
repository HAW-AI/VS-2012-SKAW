-module(starter).
-export([start/0]).
-authors("sebastian krome, andreas wimmer").
-record(configVals, {praktikumsgruppe, 
					teamno,
					koordinatorname,
					nameservicenode,
					koordinatornode}).


start() ->
	 {Praktikumsgruppe,Teamnummer,Nameservicenode,Koordinatorname} = tools:getGgtConfigData(),
	 net_adm:ping(Nameservicenode),
	 global:sync(),
	 Nameservice = global:whereis_name(nameservice),
	 io:format(pid_to_list(Nameservice)),

	 %% Koordinatoradresse beim Namensdienst erfragen

	 Nameservice ! {self(),{lookup, Koordinatorname}},

	 receive
	 	not_found -> log("Koordinator "++Koordinatorname++" not found\n");
	 	{Koordinatorname, Node} -> log("Koordinator "++atom_to_list(Koordinatorname)++" found\n"),
	 								Config = #configVals{praktikumsgruppe = Praktikumsgruppe,
	 													 teamno = Teamnummer,
	 													 koordinatorname = Koordinatorname,
	 													 nameservicenode = Nameservicenode,
	 													 koordinatornode = Node}
	 end.



log(Message) ->
	Endung = "Starter"++pid_to_list(self()),
	tools:log(Message,Endung). 