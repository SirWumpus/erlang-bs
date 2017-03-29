-module(str_test).
-include_lib("eunit/include/eunit.hrl").

at_test_() ->
	[
	?_assertMatch(badarg, str:at(<<"">>, -1)),
	?_assertMatch(badarg, str:at(<<"">>, 0)),
	?_assertMatch(badarg, str:at(<<"">>, 1)),
	?_assertMatch(badarg, str:at(<<"A">>, 2)),
	?_assertMatch($A, str:at(<<"ABC">>, 0)),
	?_assertMatch($B, str:at(<<"ABC">>, 1)),
	?_assertMatch($C, str:at(<<"ABC">>, 2))
	].

len_test_() ->
	[
	?_assertMatch(0, str:len(<<"">>)),
	?_assertMatch(1, str:len(<<"A">>)),
	?_assertMatch(2, str:len(<<"AB">>)),
	?_assertMatch(3, str:len(<<"ABC">>))
	].

chr_test_() ->
	[
	?_assertMatch(-1, str:chr(<<"">>, $A)),
	?_assertMatch(-1, str:chr(<<"ABC">>, $Z)),
	?_assertMatch(0, str:chr(<<"ABC">>, $A)),
	?_assertMatch(1, str:chr(<<"ABC">>, $B)),
	?_assertMatch(2, str:chr(<<"ABC">>, $C))
	].

rchr_test_() ->
	[
	?_assertMatch(-1, str:rchr(<<"">>, $A)),
	?_assertMatch(-1, str:rchr(<<"ABC">>, $Z)),
	?_assertMatch(0, str:rchr(<<"A">>, $A)),
	?_assertMatch(1, str:rchr(<<"AA">>, $A)),
	?_assertMatch(2, str:rchr(<<"AAA">>, $A)),
	?_assertMatch(2, str:rchr(<<"A_A">>, $A)),
	?_assertMatch(4, str:rchr(<<"A_A_A_">>, $A))
	].

cat_test_() ->
	[
	?_assertMatch(<<>>, str:cat(<<"">>, <<"">>)),
	?_assertMatch(<<"AB">>, str:cat(<<"A">>, <<"B">>)),
	?_assertMatch(<<"ABC123">>, str:cat(<<"ABC">>, <<"123">>)),

	?_assertMatch(<<>>, str:cat("", "")),
	?_assertMatch(<<"AB">>, str:cat("A", "B")),
	?_assertMatch(<<"ABC123">>, str:cat("ABC", "123"))
	].

ncat_test_() ->
	[
	?_assertMatch(badarg, str:ncat(<<"">>, <<"">>, -1)),
	?_assertMatch(<<"">>, str:ncat(<<"">>, <<"">>, 0)),
	?_assertMatch(<<"">>, str:ncat(<<"">>, <<"">>, 1)),
	?_assertMatch(<<"ABC">>, str:ncat(<<"ABC">>, <<"123">>, 0)),
	?_assertMatch(<<"ABC1">>, str:ncat(<<"ABC">>, <<"123">>, 1)),
	?_assertMatch(<<"ABC12">>, str:ncat(<<"ABC">>, <<"123">>, 2)),
	?_assertMatch(<<"ABC123">>, str:ncat(<<"ABC">>, <<"123">>, 3)),
	?_assertMatch(<<"ABC123">>, str:ncat(<<"ABC">>, <<"123">>, 4))
	].

rev_test_() ->
	[
	?_assertMatch(<<>>, str:rev(<<"">>)),
	?_assertMatch(<<"54321">>, str:rev(<<"12345">>))
	].


ltrim_test_() ->
	[
	?_assertMatch(<<>>, str:ltrim(<<"">>)),
	?_assertMatch(<<"ABC">>, str:ltrim(<<"ABC">>)),
	?_assertMatch(<<"ABC   ">>, str:ltrim(<<"   ABC   ">>)),
	?_assertMatch(<<"ABC  1 2 3   ">>, str:ltrim(<<"   ABC  1 2 3   ">>))
	].

rtrim_test_() ->
	[
	?_assertMatch(<<>>, str:rtrim(<<"">>)),
	?_assertMatch(<<"ABC">>, str:rtrim(<<"ABC">>)),
	?_assertMatch(<<"   ABC">>, str:rtrim(<<"   ABC   ">>)),
	?_assertMatch(<<"   ABC  1 2 3">>, str:rtrim(<<"   ABC  1 2 3   ">>))
	].

trim_test_() ->
	[
	?_assertMatch(<<>>, str:trim(<<"">>)),
	?_assertMatch(<<"ABC">>, str:trim(<<"ABC">>)),
	?_assertMatch(<<"ABC">>, str:trim(<<"   ABC   ">>)),
	?_assertMatch(<<"ABC  1 2 3">>, str:trim(<<"   ABC  1 2 3  ">>))
	].

spn_test_() ->
	[
	?_assertMatch(0, str:spn(<<"">>, <<"">>)),
	?_assertMatch(0, str:spn(<<"ABC">>, <<";.,">>)),
	?_assertMatch(3, str:spn(<<".,;ABC">>, <<";.,">>))
	].

cspn_test_() ->
	[
	?_assertMatch(0, str:cspn(<<"">>, <<"">>)),
	?_assertMatch(3, str:cspn(<<"ABC">>, <<";.,">>)),
	?_assertMatch(3, str:cspn(<<"ABC,123">>, <<";.,">>)),
	?_assertMatch(0, str:cspn(<<".,;ABC">>, <<";.,">>))
	].

sub_test_() ->
	[
	?_assertMatch(<<>>, str:sub(<<"">>, 0)),
	?_assertMatch(<<>>, str:sub(<<"">>, 1)),
	?_assertMatch(<<>>, str:sub(<<"ABCD123EFG">>, 4, 4)),
	?_assertMatch(<<>>, str:sub(<<"ABCD123EFG">>, 7, 4)),
	?_assertMatch(<<"ABC">>, str:sub(<<"ABCD123EFG">>, 0, 3)),
	?_assertMatch(<<"123">>, str:sub(<<"ABCD123EFG">>, 4, 7)),
	?_assertMatch(<<"EFG">>, str:sub(<<"ABCD123EFG">>, 7))
	].

tok_test_() ->
	[
	?_assertMatch({<<>>, <<>>}, str:tok(<<"">>, <<";,.">>)),
	?_assertMatch({<<"ABC">>, <<"123">>}, str:tok(<<"ABC, 123">>, <<";,. ">>)),
	?_assertMatch({<<"ABC">>, <<"123.foo">>}, str:tok(<<"ABC, 123.foo">>, <<";,. ">>)),
	?_assertMatch({<<"123">>, <<"foo">>}, str:tok(<<"123.foo">>, <<";,. ">>))
	].

cmp_test_() ->
	[
	?_assertMatch( 0, str:cmp(<<>>, <<>>)),
	?_assertMatch( 0, str:cmp(<<"A">>, <<"A">>)),
	?_assertMatch( 0, str:cmp(<<"AB">>, <<"AB">>)),
	?_assertMatch( 0, str:cmp(<<"ABC">>, <<"ABC">>)),
	?_assertMatch(-1, str:cmp(<<"A">>, <<"B">>)),
	?_assertMatch( 1, str:cmp(<<"B">>, <<"A">>)),
	?_assertMatch(-1, str:cmp(<<"A">>, <<"AB">>)),
	?_assertMatch( 1, str:cmp(<<"AB">>, <<"A">>))
	].

ncmp_test_() ->
	[
	?_assertMatch( 0, str:ncmp(<<>>, <<>>, 0)),
	?_assertMatch( 0, str:ncmp(<<>>, <<>>, 1)),
	?_assertMatch( 0, str:ncmp(<<"A">>, <<"A">>, 0)),
	?_assertMatch( 0, str:ncmp(<<"A">>, <<"A">>, 1)),
	?_assertMatch( 0, str:ncmp(<<"A">>, <<"A">>, 2)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"AB">>, 0)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"AB">>, 1)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"AB">>, 2)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"AB">>, 3)),
	?_assertMatch( 0, str:ncmp(<<"ABC">>, <<"ABC">>, 0)),
	?_assertMatch( 0, str:ncmp(<<"ABC">>, <<"ABC">>, 1)),
	?_assertMatch( 0, str:ncmp(<<"ABC">>, <<"ABC">>, 2)),
	?_assertMatch( 0, str:ncmp(<<"ABC">>, <<"ABC">>, 3)),
	?_assertMatch( 0, str:ncmp(<<"ABC">>, <<"ABC">>, 4)),
	?_assertMatch(-1, str:ncmp(<<"A">>, <<"B">>, 1)),
	?_assertMatch( 1, str:ncmp(<<"B">>, <<"A">>, 1)),
	?_assertMatch( 0, str:ncmp(<<"A">>, <<"AB">>, 0)),
	?_assertMatch( 0, str:ncmp(<<"A">>, <<"AB">>, 1)),
	?_assertMatch(-1, str:ncmp(<<"A">>, <<"AB">>, 2)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"A">>, 0)),
	?_assertMatch( 0, str:ncmp(<<"AB">>, <<"A">>, 1)),
	?_assertMatch( 1, str:ncmp(<<"AB">>, <<"A">>, 2))
	].

cpy_test_() ->
	[
	?_assertMatch(<<>>, str:cpy(<<>>)),
	?_assertMatch(<<"A">>, str:cpy(<<"A">>)),
	?_assertMatch(<<"AB">>, str:cpy(<<"AB">>)),
	?_assertMatch(<<"ABC">>, str:cpy(<<"ABC">>))
	].

ncpy_test_() ->
	[
	?_assertMatch(<<>>, str:ncpy(<<>>, 0)),
	?_assertMatch(<<>>, str:ncpy(<<>>, 1)),
	?_assertMatch(<<>>, str:ncpy(<<"A">>, 0)),
	?_assertMatch(<<"A">>, str:ncpy(<<"A">>, 1)),
	?_assertMatch(<<"A">>, str:ncpy(<<"A">>, 2)),
	?_assertMatch(<<"">>, str:ncpy(<<"AB">>, 0)),
	?_assertMatch(<<"A">>, str:ncpy(<<"AB">>, 1)),
	?_assertMatch(<<"AB">>, str:ncpy(<<"AB">>, 2)),
	?_assertMatch(<<"AB">>, str:ncpy(<<"AB">>, 3)),
	?_assertMatch(<<"">>, str:ncpy(<<"ABC">>, 0)),
	?_assertMatch(<<"A">>, str:ncpy(<<"ABC">>, 1)),
	?_assertMatch(<<"AB">>, str:ncpy(<<"ABC">>, 2)),
	?_assertMatch(<<"ABC">>, str:ncpy(<<"ABC">>, 3)),
	?_assertMatch(<<"ABC">>, str:ncpy(<<"ABC">>, 4))
	].

error_test_() ->
	[
	?_assertMatch(<<"No such file or directory.">>, str:error(enoent)),
	?_assertMatch(<<"Invalid argument.">>, str:error(einval)),
	?_assertMatch(<<"exyzzy">>, str:error(exyzzy))
	].

casecmp_test_() ->
	[
	?_assertMatch( 0, str:casecmp(<<>>, <<>>)),
	?_assertMatch( 0, str:casecmp(<<"A">>, <<"a">>)),
	?_assertMatch( 0, str:casecmp(<<"Ab">>, <<"aB">>)),
	?_assertMatch( 0, str:casecmp(<<"aBc">>, <<"AbC">>)),
	?_assertMatch(-1, str:casecmp(<<"A">>, <<"b">>)),
	?_assertMatch( 1, str:casecmp(<<"b">>, <<"A">>)),
	?_assertMatch(-1, str:casecmp(<<"A">>, <<"aB">>)),
	?_assertMatch( 1, str:casecmp(<<"aB">>, <<"A">>))
	].

ncasecmp_test_() ->
	[
	?_assertMatch( 0, str:ncasecmp(<<>>, <<>>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<>>, <<>>, 1)),
	?_assertMatch( 0, str:ncasecmp(<<"a">>, <<"A">>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<"a">>, <<"A">>, 1)),
	?_assertMatch( 0, str:ncasecmp(<<"a">>, <<"A">>, 2)),
	?_assertMatch( 0, str:ncasecmp(<<"Ab">>, <<"aB">>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<"Ab">>, <<"aB">>, 1)),
	?_assertMatch( 0, str:ncasecmp(<<"Ab">>, <<"aB">>, 2)),
	?_assertMatch( 0, str:ncasecmp(<<"Ab">>, <<"aB">>, 3)),
	?_assertMatch( 0, str:ncasecmp(<<"AbC">>, <<"aBc">>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<"AbC">>, <<"aBc">>, 1)),
	?_assertMatch( 0, str:ncasecmp(<<"AbC">>, <<"aBc">>, 2)),
	?_assertMatch( 0, str:ncasecmp(<<"AbC">>, <<"aBc">>, 3)),
	?_assertMatch( 0, str:ncasecmp(<<"AbC">>, <<"aBc">>, 4)),
	?_assertMatch(-1, str:ncasecmp(<<"A">>, <<"B">>, 1)),
	?_assertMatch( 1, str:ncasecmp(<<"B">>, <<"A">>, 1)),
	?_assertMatch( 0, str:ncasecmp(<<"A">>, <<"aB">>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<"A">>, <<"aB">>, 1)),
	?_assertMatch(-1, str:ncasecmp(<<"A">>, <<"aB">>, 2)),
	?_assertMatch( 0, str:ncasecmp(<<"aB">>, <<"A">>, 0)),
	?_assertMatch( 0, str:ncasecmp(<<"aB">>, <<"A">>, 1)),
	?_assertMatch( 1, str:ncasecmp(<<"aB">>, <<"A">>, 2))
	].

lower_test_() ->
	[
	?_assertMatch(<<>>, str:lower(<<"">>)),
	?_assertMatch(<<"abcde[123]">>, str:lower(<<"AbCdE[123]">>))
	].

upper_test_() ->
	[
	?_assertMatch(<<>>, str:lower(<<"">>)),
	?_assertMatch(<<"ABCDE[123]">>, str:upper(<<"AbCdE[123]">>))
	].

lpad_test_() ->
	[
	?_assertMatch(<<>>, str:lpad(<<>>, $., 0)),
	?_assertMatch(<<".">>, str:lpad(<<>>, $., 1)),
	?_assertMatch(<<"..">>, str:lpad(<<>>, $., 2)),
	?_assertMatch(<<"ABC">>, str:lpad(<<"ABC">>, $., 0)),
	?_assertMatch(<<"ABC">>, str:lpad(<<"ABC">>, $., 1)),
	?_assertMatch(<<"ABC">>, str:lpad(<<"ABC">>, $., 2)),
	?_assertMatch(<<"ABC">>, str:lpad(<<"ABC">>, $., 3)),
	?_assertMatch(<<".ABC">>, str:lpad(<<"ABC">>, $., 4)),
	?_assertMatch(<<"..ABC">>, str:lpad(<<"ABC">>, $., 5))
	].

rpad_test_() ->
	[
	?_assertMatch(<<>>, str:rpad(<<>>, $., 0)),
	?_assertMatch(<<".">>, str:rpad(<<>>, $., 1)),
	?_assertMatch(<<"..">>, str:rpad(<<>>, $., 2)),
	?_assertMatch(<<"ABC">>, str:rpad(<<"ABC">>, $., 0)),
	?_assertMatch(<<"ABC">>, str:rpad(<<"ABC">>, $., 1)),
	?_assertMatch(<<"ABC">>, str:rpad(<<"ABC">>, $., 2)),
	?_assertMatch(<<"ABC">>, str:rpad(<<"ABC">>, $., 3)),
	?_assertMatch(<<"ABC.">>, str:rpad(<<"ABC">>, $., 4)),
	?_assertMatch(<<"ABC..">>, str:rpad(<<"ABC">>, $., 5))
	].

ftime_test_() ->
	[
	?_assertMatch(<<>>, str:ftime(<<>>, {{2017,4,1},{17,37,46}})),
%	?_assertMatch({error, einval}, str:ftime(<<"bogus %@">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of week Sat">>, str:ftime(<<"Day of week %a">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of week Saturday">>, str:ftime(<<"Day of week %A">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Full month April">>, str:ftime(<<"Full month %B">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Short month Apr">>, str:ftime(<<"Short month %b">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Century 20">>, str:ftime(<<"Century %C">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Local date 1 Apr 2017 17:37:46">>, str:ftime(<<"Local date %c">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"American 04/01/17">>, str:ftime(<<"American %D">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Padded day 01">>, str:ftime(<<"Padded day %d">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day 1">>, str:ftime(<<"Day %e">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"ISO Date 2017-04-01">>, str:ftime(<<"ISO Date %F">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"24-hour 17">>, str:ftime(<<"24-hour %H">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"12-hour 05">>, str:ftime(<<"12-hour %I">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of year 091">>, str:ftime(<<"Day of year %j">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"\t%\n">>, str:ftime(<<"%t%%%n">>, {{2017,4,1},{17,37,46}}))
	].

tr_test_() ->
	[
	?_assertMatch(<<>>, str:tr(<<>>, <<".,;">>)),
	?_assertMatch(<<>>, str:tr(<<>>, <<".,;">>, <<"123">>)),
	?_assertMatch(<<"ABC">>, str:tr(<<"ABC">>, <<"">>, <<"123">>)),
	?_assertMatch(<<"123231312">>, str:tr(<<"!@#@#!#!@">>, <<"!@#">>, <<"123">>)),
	?_assertMatch(<<"1_2_3_">>, str:tr(<<"1!2@3#">>, <<"!@#">>, <<"_">>)),
	?_assertMatch(<<"123">>, str:tr(<<"#1##2###3">>, <<"#">>, <<"">>))
	].

