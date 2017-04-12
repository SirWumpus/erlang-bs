-module(dtz_test).
-include_lib("eunit/include/eunit.hrl").

to_epoch_seconds_test_() ->
	[
	?_assertMatch(0, dtz:to_epoch_seconds({{1970,1,1}, {0,0,0}})),
	?_assertMatch(1491090309, dtz:to_epoch_seconds({{2017,4,1}, {23,45,9}}))
	].

to_utc_seconds_test_() ->
	[
	?_assertMatch(0, dtz:to_utc_seconds({{1970,1,1}, {3,30,0}, +12600})),
	?_assertMatch(12600, dtz:to_utc_seconds({{1970,1,1}, {0,00,0}, -12600})),
	?_assertMatch(25200, dtz:to_utc_seconds({{1970,1,1}, {3,30,0}}))
	].

to_utc_test_() ->
	[
	?_assertMatch({{1970,1,1}, {0,0,0}, 0}, dtz:to_utc({{1970,1,1}, {3,30,0}, +12600})),
	?_assertMatch({{1970,1,1}, {3,30,0}, 0}, dtz:to_utc({{1970,1,1}, {0,0,0}, -12600})),
	?_assertMatch({{{1970,1,1}, {3,30,0}, 0}, <<"What?">>}, dtz:to_utc({{{1970,1,1}, {0,0,0}, -12600}, <<"What?">>}))
	].

%%
%% Assumes TZ=+03:30
%%
to_local_test_() ->
	[
	?_assertMatch({{1970,1,1}, {3,30,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, -12600})),
	?_assertMatch({{1970,1,1}, {0,0,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, 0})),
	?_assertMatch({{1969,12,31}, {23,30,0}, -12600}, dtz:to_local({{1970,1,1}, {3,30,0}, +1800})),
	?_assertMatch({{{1970,1,1}, {0,0,0}, -12600}, <<"What?">>}, dtz:to_local({{{1970,1,1}, {3,30,0}, 0}, <<"What?">>}))
	].
