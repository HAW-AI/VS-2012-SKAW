-module(tools).
-export([getClientConfigData/0,getServerConfigData/0]).

getGgtConfigData() ->
 	{ok, Configurations} = file:consult("ggt.cfg"),
 	Praktikumsgruppe = proplists:get_value(praktikumsgruppe, Configurations),
 	Teamnummer = proplists:get_value(teamnummer, Configurations),
	 Nameservicenode = proplists:get_value(nameservicenode, Configurations),
	 Koordinatorname = proplists:get_value(koordinatorname, Configurations),
	 {Praktikumsgruppe,Teamnummer,Nameservicenode,Koordinatorname}.


getKoordinatorConfigData() ->
	{ok, Configurations} = file:consult("koordinator.cfg"),
	Arbeitszeit = proplists:get_value(arbeitszeit, Configurations),
	Termzeit = proplists:get_value(termzeit, Configurations),
	Ggtprozessnummer = proplists:get_value(ggtprozessnummer, Configurations),
	Nameservicenode = proplists:get_value(nameservicenode, Configurations),
	Koordinatorname = proplists:get_value(koordinatorname, Configurations),
	{Arbeitszeit, Termzeit, Ggtprozessnummer, Nameservicenode, Koordinatorname}.


