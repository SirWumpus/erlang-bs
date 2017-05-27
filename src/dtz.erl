-module(dtz).
-export([
	to_epoch_seconds/1, time_zone_seconds/0, to_utc_seconds/1,
	to_utc/1, to_local/1, from_epoch_seconds/1
]).

to_epoch_seconds({Date = {Year, _Month, _Day}, {Hour, Min, Sec}}) ->
	Yday = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Year, 1, 1),
	Sec + Min * 60 + Hour * 3600 + Yday * 86400 + (Year - 1970) * 31536000 + ((Year - 1969) div 4) * 86400.

time_zone_seconds() ->
	Local = erlang:localtime(),
	Utc = erlang:localtime_to_universaltime(Local),
	to_epoch_seconds(Local) - to_epoch_seconds(Utc).

to_utc_seconds({Date, Time}) ->
	% Assume local time zone.
	to_utc_seconds({Date, Time, time_zone_seconds()});
to_utc_seconds({Date, Time, Tz}) ->
	if
	Tz == 0 ->
		to_epoch_seconds({Date, Time});
	Tz /= 0 ->
		to_epoch_seconds({Date, Time}) - Tz
	end.

to_utc({DTZ, Rest}) ->
	{to_utc(DTZ), Rest};
to_utc(DTZ) ->
	UTC = to_utc_seconds(DTZ),
	Timestamp = {UTC div 1000000, UTC rem 1000000, 0},
	{Date, Time} = calendar:now_to_universal_time(Timestamp),
	{Date, Time, 0}.

to_local({DTZ, Rest}) ->
	{to_local(DTZ), Rest};
to_local(DTZ) ->
	UTC = to_utc_seconds(DTZ),
	Timestamp = {UTC div 1000000, UTC rem 1000000, 0},
	{Date, Time} = calendar:now_to_local_time(Timestamp),
	{Date, Time, time_zone_seconds()}.

from_epoch_seconds(EpochSeconds) ->
	Timestamp = {EpochSeconds div 1000000, EpochSeconds rem 1000000, 0},
	{Date, Time} = calendar:now_to_universal_time(Timestamp),
	{Date, Time, 0}.
