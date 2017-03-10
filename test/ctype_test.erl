-module(ctype_test).
-include_lib("eunit/include/eunit.hrl").

isspace_test_() ->
	[
	?_assertMatch(true, ctype:isspace($ )),
	?_assertMatch(true, ctype:isspace(9)),
	?_assertMatch(true, ctype:isspace(10)),
	?_assertMatch(true, ctype:isspace(12)),
	?_assertMatch(true, ctype:isspace(13)),
	?_assertMatch(false, ctype:isspace(1)),
	?_assertMatch(false, ctype:isspace($A))
	].

isprint_test_() ->
	[
	?_assertMatch(true, ctype:isprint($ )),
	?_assertMatch(true, ctype:isprint($a)),
	?_assertMatch(true, ctype:isprint($1)),
	?_assertMatch(false, ctype:isprint(31)),
	?_assertMatch(false, ctype:isprint(127))
	].
