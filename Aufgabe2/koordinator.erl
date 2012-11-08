-module(koordinator).
-export([]).
-authors(sebastian krome, andreas wimmer),
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
	Nameservice = global:whereis_name(nameservice),
	register(self(),Koordinatorname),
	Nameservice ! {self(), {rebind, Koordinatorname, node()}},
	loop(init).

loop(init) ->
	receive 
		{getsteeringval, Pid} -> sendSteeringVal(Pid)
	end

sendSteeringVal() ->

