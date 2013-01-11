-module(utilities).
-compile(export_all).

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro) div 1000.

get_slot_for_msec(Time) ->
    erlang:trunc(((Time rem 1000)/50)).

get_current_frame() ->
    {_,CurrentFrame,_} = erlang:now(),
    CurrentFrame.

match_message(_Packet = <<_Rest:5/binary,
                         TeamNoBin:2/binary,
                         _Dash:1/binary,
                         StationBin:2/binary,
                         NutzdatenBin:14/binary,
                         SlotBin:8/integer,
                         TimestampBin:64/integer>>) ->
    Station = list_to_integer(binary_to_list(StationBin)),
    TeamNo = list_to_integer(binary_to_list(TeamNoBin)),
    Slot = SlotBin,
    Timestamp = TimestampBin,
    Nutzdaten = binary_to_list(NutzdatenBin),
    {TeamNo,Station,Slot,Nutzdaten,Timestamp}.

match_message_for_to_string(_Packet = <<PrefixBin:10/binary,
                                       NutzdatenBin:14/binary,
                                       SlotBin:8/integer,
                                       TimestampBin:64/integer>>) ->
     Prefix = binary_to_list(PrefixBin),
     Slot = SlotBin,
     Timestamp = TimestampBin,
     Nutzdaten = binary_to_list(NutzdatenBin),
     {Prefix,Slot,Nutzdaten,Timestamp}.

message_to_string(Packet) ->
     {Prefix,Slot,Nutzdaten,Timestamp} = match_message_for_to_string(Packet),
     [Prefix,Slot,Nutzdaten,Timestamp].

get_time_for_next_frame() ->
    {Mega,Sec,_} = erlang:now(),
    ((Mega * 1000000 + Sec) * 1000) + 1000.


