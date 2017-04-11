-module(sunday_test).
-include_lib("eunit/include/eunit.hrl").

sunday_test_() ->
	[
	?_assertMatch(badarg, sunday:search(<<>>, <<>>)),
	?_assertMatch(badarg, sunday:search(<<>>, <<>>, 0)),
	?_assertMatch(badarg, sunday:search(<<"ACGT">>, <<>>)),
	?_assertMatch(badarg, sunday:search(<<"ACGT">>, <<>>, 1)),
	?_assertMatch(-1, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"XYZZY">>)),
	?_assertMatch(-1, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"XYZZY">>, 0)),
	?_assertMatch(12, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>)),
	?_assertMatch(12, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>, 0)),
	?_assertMatch(12, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>, 1)),
	?_assertMatch(2, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>, 2)),
	?_assertMatch(2, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>, 3)),
	?_assertMatch(badarg, sunday:search(<<"TTAACGTAATGCAGCTA">>, <<"AGCT">>, 4)),
	?_assertMatch(16, sunday:search(<<"GCATCGCAGAGCGTATGCAGAGAG">>, <<"GCAGAGAG">>, 0)),
	?_assertMatch(5, sunday:search(<<"GCATCGCAGAGCGTATGCAGAGAG">>, <<"GCAGAGAG">>, 1)),
	?_assertMatch(-1, sunday:search(<<"CAGAGAGTATGCAGAGCG">>, <<"GCAGAGAG">>, 0)),
	?_assertMatch(10, sunday:search(<<"CAGAGAGTATGCAGAGCG">>, <<"GCAGAGAG">>, 1)),
	?_assertMatch(0, sunday:search(<<"GCAGAGAG">>, <<"GCAGAGAG">>, 1))
	].
