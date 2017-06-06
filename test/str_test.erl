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
	?_assertMatch({<<"123">>, <<"foo">>}, str:tok(<<", 123.foo">>, <<";,. ">>)),
	?_assertMatch({<<"foo">>, <<"">>}, str:tok(<<".foo">>, <<";,. ">>))
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

pad_int_test_() ->
	[
	?_assertMatch(<<"0">>, str:pad_int(0, $., 0)),
	?_assertMatch(<<"1">>, str:pad_int(1, $., 1)),
	?_assertMatch(<<".2">>, str:pad_int(2, $., 2)),
	?_assertMatch(<<"..3">>, str:pad_int(3, $., 3)),
	?_assertMatch(<<"...4">>, str:pad_int(4, $., 4)),
	?_assertMatch(<<"12345">>, str:pad_int(12345, $., 3)),
	?_assertMatch(<<"....12345">>, str:pad_int(12345, $., 9)),
	?_assertMatch(<<"-1">>, str:pad_int(-1, $., 1)),
	?_assertMatch(<<"-2">>, str:pad_int(-2, $., 2)),
	?_assertMatch(<<".-3">>, str:pad_int(-3, $., 3)),
	?_assertMatch(<<"..-4">>, str:pad_int(-4, $., 4)),
	?_assertMatch(<<"-12345">>, str:pad_int(-12345, $., 3)),
	?_assertMatch(<<"...-12345">>, str:pad_int(-12345, $., 9)),
	?_assertMatch(<<"0">>, str:pad_int(0, $0, 0)),
	?_assertMatch(<<"1">>, str:pad_int(1, $0, 1)),
	?_assertMatch(<<"02">>, str:pad_int(2, $0, 2)),
	?_assertMatch(<<"003">>, str:pad_int(3, $0, 3)),
	?_assertMatch(<<"0004">>, str:pad_int(4, $0, 4)),
	?_assertMatch(<<"12345">>, str:pad_int(12345, $0, 3)),
	?_assertMatch(<<"000012345">>, str:pad_int(12345, $0, 9)),
	?_assertMatch(<<"-1">>, str:pad_int(-1, $0, 1)),
	?_assertMatch(<<"-2">>, str:pad_int(-2, $0, 2)),
	?_assertMatch(<<"-03">>, str:pad_int(-3, $0, 3)),
	?_assertMatch(<<"-004">>, str:pad_int(-4, $0, 4)),
	?_assertMatch(<<"-12345">>, str:pad_int(-12345, $0, 3)),
	?_assertMatch(<<"-00012345">>, str:pad_int(-12345, $0, 9))
	].

pad_sign_int_test_() ->
	[
	?_assertMatch(<<"+0">>, str:pad_sign_int(0, $., 0)),
	?_assertMatch(<<"+1">>, str:pad_sign_int(1, $., 1)),
	?_assertMatch(<<"+2">>, str:pad_sign_int(2, $., 2)),
	?_assertMatch(<<".+3">>, str:pad_sign_int(3, $., 3)),
	?_assertMatch(<<"..+4">>, str:pad_sign_int(4, $., 4)),
	?_assertMatch(<<"+12345">>, str:pad_sign_int(12345, $., 3)),
	?_assertMatch(<<"...+12345">>, str:pad_sign_int(12345, $., 9)),
	?_assertMatch(<<"-1">>, str:pad_sign_int(-1, $., 1)),
	?_assertMatch(<<"-2">>, str:pad_sign_int(-2, $., 2)),
	?_assertMatch(<<".-3">>, str:pad_sign_int(-3, $., 3)),
	?_assertMatch(<<"..-4">>, str:pad_sign_int(-4, $., 4)),
	?_assertMatch(<<"-12345">>, str:pad_sign_int(-12345, $., 3)),
	?_assertMatch(<<"...-12345">>, str:pad_sign_int(-12345, $., 9)),
	?_assertMatch(<<"+0">>, str:pad_sign_int(0, $0, 0)),
	?_assertMatch(<<"+1">>, str:pad_sign_int(1, $0, 1)),
	?_assertMatch(<<"+2">>, str:pad_sign_int(2, $0, 2)),
	?_assertMatch(<<"+03">>, str:pad_sign_int(3, $0, 3)),
	?_assertMatch(<<"+004">>, str:pad_sign_int(4, $0, 4)),
	?_assertMatch(<<"+12345">>, str:pad_sign_int(12345, $0, 3)),
	?_assertMatch(<<"+00012345">>, str:pad_sign_int(12345, $0, 9)),
	?_assertMatch(<<"-1">>, str:pad_sign_int(-1, $0, 1)),
	?_assertMatch(<<"-2">>, str:pad_sign_int(-2, $0, 2)),
	?_assertMatch(<<"-03">>, str:pad_sign_int(-3, $0, 3)),
	?_assertMatch(<<"-004">>, str:pad_sign_int(-4, $0, 4)),
	?_assertMatch(<<"-12345">>, str:pad_sign_int(-12345, $0, 3)),
	?_assertMatch(<<"-00012345">>, str:pad_sign_int(-12345, $0, 9))
	].

ftime_test_() ->
	[
	?_assertMatch(<<>>, str:ftime(<<>>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, einval}, str:ftime(<<"bogus %@">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of week Sat">>, str:ftime(<<"Day of week %a">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of week Saturday">>, str:ftime(<<"Day of week %A">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Full month April">>, str:ftime(<<"Full month %B">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Short month Apr">>, str:ftime(<<"Short month %b">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Short month Apr">>, str:ftime(<<"Short month %h">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Century 20">>, str:ftime(<<"Century %C">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Local date  1 Apr 2017 17:37:46">>, str:ftime(<<"Local date %c">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"American 04/01/17">>, str:ftime(<<"American %D">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Padded day 01">>, str:ftime(<<"Padded day %d">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day  1">>, str:ftime(<<"Day %e">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"ISO Date 2017-04-01">>, str:ftime(<<"ISO Date %F">>, {{2017,4,1},{17,37,9}})),
	?_assertMatch(<<"24-hour 17">>, str:ftime(<<"24-hour %H">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"12-hour 05">>, str:ftime(<<"12-hour %I">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Day of year 091">>, str:ftime(<<"Day of year %j">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"24-hour [ 3]">>, str:ftime(<<"24-hour [%k]">>, {{2017,4,1},{3,37,46}})),
	?_assertMatch(<<"12-hour [ 5]">>, str:ftime(<<"12-hour [%l]">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"17:37">>, str:ftime(<<"%R">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<" 5:37 am">>, str:ftime(<<"%r">>, {{2017,4,1},{5,37,46}})),
	?_assertMatch(<<" 5:37 pm">>, str:ftime(<<"%r">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"ISO Time 17:37:09">>, str:ftime(<<"ISO Time %T">>, {{2017,4,1},{17,37,9}})),
	?_assertMatch(<<"Week 13">>, str:ftime(<<"Week %V">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Date  1-Apr-2017">>, str:ftime(<<"Date %v">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"Epoch 1491082666">>, str:ftime(<<"Epoch %s">>, {{2017,4,1},{17,37,46},-14400})),
	?_assertMatch(<<"UTC +0000">>, str:ftime(<<"UTC %z">>, {{2017,4,1},{17,37,46}, 0})),
	?_assertMatch(<<"Paris +0100">>, str:ftime(<<"Paris %z">>, {{2017,4,1},{17,37,46}, 3600})),
	?_assertMatch(<<"Ottawa -0500">>, str:ftime(<<"Ottawa %z">>, {{2017,4,1},{17,37,46}, -18000})),
	?_assertMatch(<<"St. Johns, NL -0330">>, str:ftime(<<"St. Johns, NL %z">>, {{2017,4,1},{17,37,46}, -12600})),
	?_assertMatch(<<"\t%\n">>, str:ftime(<<"%t%%%n">>, {{2017,4,1},{17,37,46}})),
	?_assertMatch(<<"20170527T175300Z">>, str:ftime(<<"%Y%m%dT%H%M%SZ">>, 1495907580)),
	?_assertThrow({error, not_supported}, str:ftime(<<"%G">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%g">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%U">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%u">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%W">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%w">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%X">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%x">>, {{2017,4,1},{17,37,46}})),
	?_assertThrow({error, not_supported}, str:ftime(<<"%Z">>, {{2017,4,1},{17,37,46}}))
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

to_int_test_() ->
	[
	?_assertMatch(badarg, str:to_int(<<>>, -1)),
	?_assertMatch(badarg, str:to_int(<<>>, 37)),
	?_assertMatch(badarg, str:to_int(<<"123">>, -1)),
	?_assertMatch(badarg, str:to_int(<<"123">>, 37)),
	?_assertMatch(badarg, str:to_int(<<"no digits">>, 10)),
	?_assertMatch({2, <<>>}, str:to_int(<<"10">>, 2)),
	?_assertMatch({9, <<>>}, str:to_int(<<"1001">>, 2)),
	?_assertMatch({999, <<>>}, str:to_int(<<"  00999">>, 10)),
	?_assertMatch({-999, <<>>}, str:to_int(<<"  -00999">>, 10)),
	?_assertMatch({+999, <<>>}, str:to_int(<<"  +00999">>, 10)),
	?_assertMatch({16#deadbeef, <<>>}, str:to_int(<<"000DEADbeef">>, 16)),
	?_assertMatch({16#beef1234, <<>>}, str:to_int(<<"0x000beef1234">>, 16)),
	?_assertMatch({36#deathmatch, <<>>}, str:to_int(<<"000DeathMatch">>, 36)),
	?_assertMatch({255, <<":foobar">>}, str:to_int(<<"0377:foobar">>, 0)),
	?_assertMatch({16#abcd, <<":foobar">>}, str:to_int(<<"0xAbCd:foobar">>, 0)),
	?_assertMatch({999, <<":foobar">>}, str:to_int(<<"999:foobar">>, 0)),
	?_assertMatch({-999, <<":foobar">>}, str:to_int(<<"-00999:foobar">>, 0)),
	?_assertMatch({+999, <<":foobar">>}, str:to_int(<<"+00999:foobar">>, 0))
	].

iso_date_time_test_() ->
	[
	?_assertMatch(badarg, str:iso_date_time(<<>>)),
	?_assertMatch(badarg, str:iso_date_time(<<" boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0}, _Tz}, <<"--boo!">>}, str:iso_date_time(<<"20170401--boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"20170401 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"2017-04-01 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<"">>}, str:iso_date_time(<<"20170401T180923">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:iso_date_time(<<"20170401T18:09:23Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:iso_date_time(<<"2017-04-01T18:09:23Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923.234Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923,234Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"20170401T18:09:23 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"2017-04-01T18:09:23 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923.234 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923.234 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923-0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923.234-0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 12600}, <<" boo!">>}, str:iso_date_time(<<"20170401T180923.234+0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:iso_date_time(<<"2017-04-01T18:09:23.234-03:30 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 12600}, <<" boo!">>}, str:iso_date_time(<<"2017-04-01T18:09:23.234+03:30 boo!">>))
	].

ptime_test_() ->
	[
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<>>, <<>>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<"boo!">>}, str:ptime(<<"boo!">>,<<>>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<" boo!">>,<<" boo!">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"[%]">>,<<"[%%]">>)),
	?_assertMatch({badarg, <<"-]">>}, str:ptime(<<"[-]">>,<<"[%%]">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"abc \t \f boo!">>,<<"abc boo!">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<"boo!">>}, str:ptime(<<" \t\fboo!">>,<<"%n">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<"boo!">>}, str:ptime(<<" \t\fboo!">>,<<"%t">>)),
	?_assertMatch({badarg, <<"Antday">>}, str:ptime(<<"Antday">>,<<"%A">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"Sat">>,<<"%a">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"Monday">>,<<"%A">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"WED">>,<<"%a">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"friday">>,<<"%A">>)),
	?_assertMatch({badarg, <<"Howember">>}, str:ptime(<<"Howember">>,<<"%b">>)),
	?_assertMatch({{{_,9,_},{0,0,0},_Tz}, <<>>}, str:ptime(<<"Sep">>,<<"%b">>)),
	?_assertMatch({{{_,9,_},{0,0,0},_Tz}, <<>>}, str:ptime(<<"Sep">>,<<"%h">>)),
	?_assertMatch({{{_,9,_},{0,0,0},_Tz}, <<>>}, str:ptime(<<"September">>,<<"%B">>)),
	?_assertMatch({badarg, <<"xx">>}, str:ptime(<<"xx">>,<<"%C">>)),
	?_assertMatch({{{2017,_,_},{0,0,0},_Tz}, <<>>}, str:ptime(<<"2017">>,<<"%C%y">>)),
	?_assertMatch({{{2017,4,1},{20,35,43},_Tz}, <<>>}, str:ptime(<<"1 Apr 2017 20:35:43">>,<<"%c">>)),
	?_assertMatch({badarg, <<"0">>}, str:ptime(<<"0">>,<<"%d">>)),
	?_assertMatch({badarg, <<"32">>}, str:ptime(<<"32">>,<<"%d">>)),
	?_assertMatch({{{_,_,31},{0,0,0},_Tz}, <<>>}, str:ptime(<<"31">>,<<"%d">>)),
	?_assertMatch({{{2017,4,1},{0,0,0},_Tz}, <<>>}, str:ptime(<<"4/1/17">>,<<"%D">>)),
	?_assertMatch({{{2017,4,1},{0,0,0},_Tz}, <<>>}, str:ptime(<<"17/4/1">>,<<"%y/%m/%d">>)),
	?_assertMatch({{{1984,4,1},{0,0,0},_Tz}, <<>>}, str:ptime(<<"84/4/1">>,<<"%y/%m/%d">>)),
	?_assertMatch({{{2017,4,1},{0,0,0},_Tz}, <<>>}, str:ptime(<<"2017-04-01">>,<<"%F">>)),
	?_assertMatch({{_, {23, 0, 0}, _Tz}, <<>>}, str:ptime(<<"23">>,<<"%k">>)),
	?_assertMatch({{_, {21, 0, 0}, _Tz}, <<>>}, str:ptime(<<"09 PM">>,<<"%I %p">>)),
	?_assertMatch({{_, {23, 0, 0}, _Tz}, <<>>}, str:ptime(<<"23 PM">>,<<"%H %p">>)),
	?_assertMatch({badarg, <<"-1">>}, str:ptime(<<"-1">>,<<"%H">>)),
	?_assertMatch({badarg, <<"24">>}, str:ptime(<<"24">>,<<"%H">>)),
	?_assertMatch({badarg, <<"0">>}, str:ptime(<<"0">>,<<"%I">>)),
	?_assertMatch({badarg, <<"13">>}, str:ptime(<<"13">>,<<"%I">>)),
	?_assertMatch({badarg, <<"-1">>}, str:ptime(<<"-1">>,<<"%M">>)),
	?_assertMatch({badarg, <<"60">>}, str:ptime(<<"60">>,<<"%M">>)),
	?_assertMatch({badarg, <<"60">>}, str:ptime(<<"60">>,<<"%M">>)),
	?_assertMatch({badarg, <<"0">>}, str:ptime(<<"0">>,<<"%m">>)),
	?_assertMatch({badarg, <<"13">>}, str:ptime(<<"13">>,<<"%m">>)),
	?_assertMatch({badarg, <<"-1">>}, str:ptime(<<"-1">>,<<"%S">>)),
	?_assertMatch({badarg, <<"62">>}, str:ptime(<<"62">>,<<"%S">>)),
	?_assertMatch({{{_,9,_},{0,0,0},_Tz}, <<>>}, str:ptime(<<"9">>,<<"%m">>)),
	?_assertMatch({badarg, <<"000">>}, str:ptime(<<"000">>,<<"%j">>)),
	?_assertMatch({badarg, <<"666">>}, str:ptime(<<"666">>,<<"%j">>)),
	?_assertMatch({{_,{0,0,0},_Tz}, <<>>}, str:ptime(<<"095">>,<<"%j">>)),
	?_assertMatch({{_,{8,35,0},_Tz}, <<>>}, str:ptime(<<"8:35 am">>,<<"%r">>)),
	?_assertMatch({{_,{20,35,0},_Tz}, <<>>}, str:ptime(<<"8:35 PM">>,<<"%r">>)),
	?_assertMatch({badarg, <<"ibm">>}, str:ptime(<<"ibm">>,<<"%p">>)),
	?_assertMatch({{_,{20,35,0},_Tz}, <<>>}, str:ptime(<<"20:35">>,<<"%R">>)),
	?_assertMatch({{{2017,4,6},{00,55,20},0}, <<>>}, dtz:to_utc(str:ptime(<<"1491440120">>,<<"%s">>))),
	?_assertMatch({{_,{20,35,43},_Tz}, <<>>}, str:ptime(<<"20:35:43">>,<<"%T">>)),
	?_assertMatch({badarg, <<"boo!">>}, str:ptime(<<"boo!">>,<<"%z">>)),
	?_assertMatch({{_,{0,0,0},-12600}, <<>>}, str:ptime(<<"-0330">>,<<"%z">>)),
	?_assertMatch({{_,{20,35,43},_Tz}, <<" boo!">>}, str:ptime(<<"20:35:43 boo!">>,<<"%T">>)),
	?_assertMatch({badarg, <<" boo!">>}, str:ptime(<<"20:35:43 boo!">>,<<"%T%z">>)),
	?_assertMatch({badarg, <<" boo!">>}, str:ptime(<<"20:35:43 boo!">>,<<"%T %z">>)),
	?_assertMatch({{_,{0,0,0},-12600}, <<" boo!">>}, str:ptime(<<"-0330 boo!">>,<<"%z">>)),
	?_assertMatch({{_,{20,35,43},-12600}, <<" boo!">>}, str:ptime(<<"20:35:43 -0330 boo!">>,<<"%T %z">>))
	].

to_date_time_test_() ->
	[
	?_assertMatch(badarg, str:to_date_time(<<>>)),
	?_assertMatch(badarg, str:to_date_time(<<"boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0}, _Tz}, <<" boo!">>}, str:to_date_time(<<"20170401 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0}, _Tz}, <<" boo!">>}, str:to_date_time(<<"2017-04-01 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, 0}, <<" boo!">>}, str:to_date_time(<<"20170401T180923Z boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:to_date_time(<<"20170401T180923.234 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"20170401T180923.234-0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"2017-04-01T18:09:23.234-03:30 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:to_date_time(<<"1 Apr 2017 18:09:23 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"1 Apr 2017 18:09:23 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Sat 1 Apr 2017 18:09:23 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Sat, 1 Apr 2017 18:09:23 -03:30 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Saturday, 1 April 2017 18:09:23 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23}, _Tz}, <<" boo!">>}, str:to_date_time(<<"Apr 1 18:09:23 2017 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Apr 1 18:09:23 2017 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Sat Apr 1 18:09:23 2017 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Sat, Apr 1 18:09:23 2017 -03:30 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{18, 09, 23},-12600}, <<" boo!">>}, str:to_date_time(<<"Saturday, April 1 18:09:23 2017 -0330 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0},_Tz}, <<" boo!">>}, str:to_date_time(<<"Apr 1, 2017 boo!">>)),
	?_assertMatch({{{2017, 4, 1},{0, 0, 0},_Tz}, <<" boo!">>}, str:to_date_time(<<"April 1 2017 boo!">>)),
	?_assertMatch({{_,{15, 30, 0},_Tz}, <<" boo!">>}, str:to_date_time(<<"15:30 boo!">>)),
	?_assertMatch({{_,{15, 30, 0},_Tz}, <<" boo!">>}, str:to_date_time(<<"1530 boo!">>))
	].

str_test_() ->
	[
        ?_assertMatch(0, str:str(<<>>, <<>>)),
        ?_assertMatch(0, str:str(<<"Joey Bloggs">>, <<>>)),
        ?_assertMatch(0, str:str(<<"Joey Bloggs">>, <<"Joey">>)),
        ?_assertMatch(5, str:str(<<"Joey Bloggs">>, <<"Blog">>)),
        ?_assertMatch(8, str:str(<<"Joey Bloggs">>, <<"ggs">>)),
        ?_assertMatch(-1, str:str(<<"Joey Bloggs">>, <<"ABC">>)),
        ?_assertMatch(-1, str:str(<<"Joey Bloggs">>, <<"blog">>)),
        ?_assertMatch(-1, str:str(<<"Joey Bloggs">>, <<"BLOG">>))
	].

casestr_test_() ->
	[
        ?_assertMatch(0, str:casestr(<<>>, <<>>)),
        ?_assertMatch(0, str:casestr(<<"Joey Bloggs">>, <<>>)),
        ?_assertMatch(0, str:casestr(<<"Joey Bloggs">>, <<"Joey">>)),
        ?_assertMatch(5, str:casestr(<<"Joey Bloggs">>, <<"Blog">>)),
        ?_assertMatch(8, str:casestr(<<"Joey Bloggs">>, <<"GGS">>)),
        ?_assertMatch(-1, str:casestr(<<"Joey Bloggs">>, <<"ABC">>)),
        ?_assertMatch(5, str:casestr(<<"Joey Bloggs">>, <<"blog">>)),
        ?_assertMatch(5, str:casestr(<<"Joey Bloggs">>, <<"BLOG">>))
	].

isprintable_test_() ->
	[
        ?_assertMatch(false, str:isprintable([])),
        ?_assertMatch(false, str:isprintable("")),
        ?_assertMatch(false, str:isprintable(123)),
        ?_assertMatch(false, str:isprintable({a, b})),
        ?_assertMatch(true, str:isprintable(<<>>)),
        ?_assertMatch(true, str:isprintable(<<"abc 123 !@#">>)),
        ?_assertMatch(true, str:isprintable(<<"beep\b tab\t esc\e">>)),
        ?_assertMatch(false, str:isprintable(<<"abc", 1, 2, 3, "bar">>))
	].

token_strip_test_() ->
	[
	% Empty string.
	?_assertMatch({<<>>, <<>>}, str:token(<<"">>)),

	% Default whitespace delimiters.
	?_assertMatch({<<"ABC">>, <<"123">>}, str:token(<<"ABC 123">>)),
	?_assertMatch({<<"ABC">>, <<"\t123">>}, str:token(<<"ABC\t\t123">>)),
	?_assertMatch({<<>>, <<"123">>}, str:token(<<"\t123">>)),

	% Supplied delimiters.
	?_assertMatch({<<"ABC">>, <<" 123">>}, str:token(<<"ABC, 123">>, <<";,.">>)),
	?_assertMatch({<<" 123">>, <<>>}, str:token(<<" 123">>, <<";,.">>)),
	?_assertMatch({<<"ABC">>, <<" 123.foo">>}, str:token(<<"ABC, 123.foo">>, <<";,.">>)),
	?_assertMatch({<<"">>, <<" 123.foo">>}, str:token(<<", 123.foo">>, <<";,.">>)),

	% Single quoted string
	?_assertMatch({<<>>, <<>>}, str:token(<<"''">>)),
	?_assertMatch({<<"ABC">>, <<>>}, str:token(<<"'ABC'">>)),
	?_assertMatch({<<>>, <<"123">>}, str:token(<<"'' 123">>)),
	?_assertMatch({<<$'>>, <<>>}, str:token(<<"'\\\''">>)),
	?_assertMatch({<<"ABC ' 123">>, <<>>}, str:token(<<"ABC' \\\' '123">>)),
	?_assertThrow({error, unbalanced_quotes, $', <<"ABC123">>}, str:token(<<"ABC'123">>)),
	?_assertMatch({<<"ABC'123">>, <<>>}, str:token(<<"ABC\\'123">>)),

	% Double quoted string
	?_assertMatch({<<>>, <<>>}, str:token(<<"\"\"">>)),
	?_assertMatch({<<"ABC">>, <<>>}, str:token(<<"\"ABC\"">>)),
	?_assertMatch({<<>>, <<"123">>}, str:token(<<"\"\" 123">>)),
	?_assertMatch({<<$">>, <<>>}, str:token(<<"\"\\\"\"">>)),
	?_assertMatch({<<"ABC \" 123">>, <<>>}, str:token(<<"ABC\" \\\" \"123">>)),
	?_assertThrow({error, unbalanced_quotes, $", <<"ABC123">>}, str:token(<<"ABC\"123">>)),
	?_assertMatch({<<"ABC\"123">>, <<>>}, str:token(<<"ABC\\\"123">>))
	].

token_keep_test_() ->
	[
	% Empty string.
	?_assertMatch({<<>>, <<>>}, str:token(<<"">>, <<" \t\r\n\f">>, true)),

	% Single quoted string
	?_assertMatch({<<"''">>, <<>>}, str:token(<<"''">>, <<" \t\r\n\f">>, true)),
	?_assertMatch({<<"'ABC'">>, <<>>}, str:token(<<"'ABC'">>, <<" \t\r\n\f">>, true)),
	?_assertMatch({<<"''">>, <<"123">>}, str:token(<<"'' 123">>, <<" \t\r\n\f">>, true)),
	?_assertMatch({<<"'\\\''">>, <<>>}, str:token(<<"'\\\''">>, <<" \t\r\n\f">>, true)),
	?_assertMatch({<<"ABC' \\\' '123">>, <<>>}, str:token(<<"ABC' \\\' '123">>, <<" \t\r\n\f">>, true))
	].
