-module(dtz).
-export([
	to_epoch_seconds/1, from_epoch_seconds/1, time_zone_seconds/0, to_utc/1, to_local/1
]).

-type usec()		:: 0..999999.			% Micro seconds.
-type tz()		:: integer().			% Offset seconds from UTC; 0 = UTC.
-type isotime()		:: {calendar:hour(), calendar:minute(), calendar:second(), usec()}.
-type dtz()		:: {calendar:date(), calendar:time(), tz()}.
-type dtz0()		:: dtz() | {{0,0,0},{0,0,0},integer()}.
-type utc()		:: {calendar:date(), calendar:time(), 0}.
-type epoch()		:: {{1970, 1, 1},{0, 0 ,0}, 0}.
-type epochsecs()	:: integer().

-export_type([
	usec/0,
	tz/0,
	isotime/0,
	dtz/0,
	dtz0/0,
	utc/0,
	epoch/0,
	epochsecs/0
]).

-spec to_epoch_seconds(dtz() | calendar:datetime()) -> epochsecs().
to_epoch_seconds({Date, Time}) ->
	% Without a timezone, assume UTC.
	to_epoch_seconds({Date, Time, 0});

to_epoch_seconds({Date = {Year, _Month, _Day}, {Hour, Min, Sec}, Tz}) ->
	Yday = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Year, 1, 1),
	Sec + Min * 60 + Hour * 3600 + Yday * 86400 + (Year - 1970) * 31536000 + ((Year - 1969) div 4) * 86400 - Tz.

-spec time_zone_seconds() -> tz().
time_zone_seconds() ->
	% This relies on the system timezone or a correctly specified $TZ.
	Local = erlang:localtime(),
	Utc = erlang:localtime_to_universaltime(Local),
	to_epoch_seconds(Local) - to_epoch_seconds(Utc).

-spec to_utc(dtz()) -> utc().
to_utc(Dtz) ->
	Epoch = to_epoch_seconds(Dtz),
	from_epoch_seconds(Epoch).

-spec to_local(dtz()) -> dtz().
to_local(Dtz) ->
	Epoch = to_epoch_seconds(Dtz) + time_zone_seconds(),
	{Date, Time, 0} = from_epoch_seconds(Epoch),
	{Date, Time, time_zone_seconds()}.

-spec from_epoch_seconds(epochsecs()) -> utc().
from_epoch_seconds(EpochSeconds) ->
	Timestamp = {EpochSeconds div 1000000, EpochSeconds rem 1000000, 0},
	{Date, Time} = calendar:now_to_universal_time(Timestamp),
	{Date, Time, 0}.
