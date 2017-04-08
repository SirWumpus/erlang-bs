-module(dtz_test).
-include_lib("eunit/include/eunit.hrl").

to_epoch_seconds_test_() ->
	[
	?_assertMatch(0, dtz:to_epoch_seconds({{1970,1,1}, {0,0,0}})),
	?_assertMatch(1491090309, dtz:to_epoch_seconds({{2017,4,1}, {23,45,9}}))
	].

to_utc_seconds_test_() ->
	[
	?_assertMatch(0, dtz:to_utc_seconds({{1970,1,1}, {3,30,0}, +12600}))
	].

to_utc_test_() ->
	[
	?_assertMatch({{1970,1,1}, {0,0,0}, 0}, dtz:to_utc({{1970,1,1}, {3,30,0}, +12600}))
	].
