-module(dtz_test).
-include_lib("eunit/include/eunit.hrl").

%%
%% Assumes TZ=NST+03:30
%%
time_zone_seconds_test_() ->
	[
	?_assertMatch(-12600, dtz:time_zone_seconds())
	].

%%
%% Assumes TZ=NST+03:30
%%
to_epoch_seconds_test_() ->
	[
	?_assertMatch(0, dtz:to_epoch_seconds({{1970,1,1}, {0,0,0}})),
	?_assertMatch(1491090309, dtz:to_epoch_seconds({{2017,4,1}, {23,45,9}})),
	?_assertMatch(0, dtz:to_epoch_seconds({{1970,1,1}, {3,30,0}, +12600})),
	?_assertMatch(12600, dtz:to_epoch_seconds({{1970,1,1}, {0,00,0}, -12600})),
	?_assertMatch(0, dtz:to_epoch_seconds({{1969,12,31}, {20,30,0}, dtz:time_zone_seconds()}))
	].

to_utc_test_() ->
	[
	?_assertMatch({{1970,1,1}, {0,0,0}, 0}, dtz:to_utc({{1970,1,1}, {3,30,0}, +12600})),
	?_assertMatch({{1970,1,1}, {3,30,0}, 0}, dtz:to_utc({{1970,1,1}, {0,0,0}, -12600}))
	].

%%
%% Assumes TZ=NST+03:30
%%
to_local_test_() ->
	[
	?_assertMatch({{1970,1,1}, {0,0,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, 0})),
	?_assertMatch({{1970,1,1}, {3,30,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, -12600})),
	?_assertMatch({{1969,12,31}, {23,0,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, +3600}))
	].

from_epoch_seconds_test_() ->
	[
	?_assertMatch({{1970,1,1}, {0,0,0}, 0}, dtz:from_epoch_seconds(0)),
	?_assertMatch({{2017,5,27}, {17,53,0}, 0}, dtz:from_epoch_seconds(1495907580)),
	?_assertMatch({{2017,4,1}, {23,45,9}, 0}, dtz:from_epoch_seconds(dtz:to_epoch_seconds({{2017,4,1}, {23,45,9}})))
	].
