-module(koordinator).
-export([start/0]).
-authors("sebastian krome, andreas wimmer").
-record(steeringVals, {arbeitszeit, 
						termzeit,
						ggtProzessnummer,
						nameservicenode,
						koordinatorname}).

start() ->
	{Arbeitszeit, Termzeit, Ggtprozessnummer, Nameservicenode, Koordinatorname} = tools:getKoordinatorConfigData(),
	SteeringVals = #steeringVals{arbeitszeit = Arbeitszeit,
								termzeit = Termzeit,
								ggtProzessnummer = Ggtprozessnummer,
								nameservicenode = Nameservicenode,
								koordinatorname = Koordinatorname},
	net_adm:ping(Nameservicenode),
	global:sync(),
	Nameservice = global:whereis_name(nameservice),
	register(Koordinatorname,self()),
	Nameservice ! {self(), {rebind, Koordinatorname, node()}},
	loop(init, SteeringVals).

loop(init, SteeringVals) ->
	receive 
		{getsteeringval, Pid} -> Pid ! {steeringval,
										SteeringVals#steeringVals.arbeitszeit,
										SteeringVals#steeringVals.termzeit,
										SteeringVals#steeringVals.ggtProzessnummer}
	end.

