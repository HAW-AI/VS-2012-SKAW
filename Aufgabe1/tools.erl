-module(tools).
-export([getClientConfigData/0,getServerConfigData/0]).

getClientConfigData() ->
 	{ok, Configurations} = file:consult("client.cfg"),
 	Clients = proplists:get_value(clients, Configurations),
 	Lifetime = proplists:get_value(lifetime, Configurations),
	 Servername = proplists:get_value(servername, Configurations),
	 Intervall = proplists:get_value(sendeintervall, Configurations),
	 {Clients, Lifetime, Servername, Intervall}.


getServerConfigData() ->
	{ok, Configurations} = file:consult("server.cfg"),
	Lifetime = proplists:get_value(lifetime, Configurations),
	RememberTime = proplists:get_value(clientlifetime, Configurations),
	Servername = proplists:get_value(servername, Configurations),
	DLQ_limit = proplists:get_value(dlqlimit, Configurations),
	Difftime = proplists:get_value(difftime, Configurations),
	{Lifetime,RememberTime,Servername,DLQ_limit,Difftime}.


