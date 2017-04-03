-module(ctype_test).
-include_lib("eunit/include/eunit.hrl").

isblank_test_() ->
	[
	?_assertMatch(true, ctype:isblank($ )),
	?_assertMatch(true, ctype:isblank(9)),
	?_assertMatch(false, ctype:isblank(10)),
	?_assertMatch(false, ctype:isblank(12)),
	?_assertMatch(false, ctype:isblank(13)),
	?_assertMatch(false, ctype:isblank(1)),
	?_assertMatch(false, ctype:isblank($A))
	].

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

isdigit_test_() ->
	[
	?_assertMatch(true, ctype:isdigit($1)),
	?_assertMatch(true, ctype:isdigit($2)),
	?_assertMatch(true, ctype:isdigit($3)),
	?_assertMatch(true, ctype:isdigit($4)),
	?_assertMatch(true, ctype:isdigit($5)),
	?_assertMatch(true, ctype:isdigit($8)),
	?_assertMatch(true, ctype:isdigit($9)),
	?_assertMatch(true, ctype:isdigit($0)),
	?_assertMatch(false, ctype:isdigit($-)),
	?_assertMatch(false, ctype:isdigit($ )),
	?_assertMatch(false, ctype:isdigit($a)),
	?_assertMatch(false, ctype:isdigit(31)),
	?_assertMatch(false, ctype:isdigit(127))
	].

isxdigit_test_() ->
	[
	?_assertMatch(true, ctype:isxdigit($1)),
	?_assertMatch(true, ctype:isxdigit($2)),
	?_assertMatch(true, ctype:isxdigit($3)),
	?_assertMatch(true, ctype:isxdigit($4)),
	?_assertMatch(true, ctype:isxdigit($5)),
	?_assertMatch(true, ctype:isxdigit($8)),
	?_assertMatch(true, ctype:isxdigit($9)),
	?_assertMatch(true, ctype:isxdigit($A)),
	?_assertMatch(true, ctype:isxdigit($B)),
	?_assertMatch(true, ctype:isxdigit($C)),
	?_assertMatch(true, ctype:isxdigit($D)),
	?_assertMatch(true, ctype:isxdigit($E)),
	?_assertMatch(true, ctype:isxdigit($F)),
	?_assertMatch(true, ctype:isxdigit($a)),
	?_assertMatch(true, ctype:isxdigit($b)),
	?_assertMatch(true, ctype:isxdigit($c)),
	?_assertMatch(true, ctype:isxdigit($d)),
	?_assertMatch(true, ctype:isxdigit($e)),
	?_assertMatch(true, ctype:isxdigit($f)),
	?_assertMatch(false, ctype:isxdigit($-)),
	?_assertMatch(false, ctype:isxdigit($ )),
	?_assertMatch(false, ctype:isxdigit($Z)),
	?_assertMatch(false, ctype:isxdigit(31)),
	?_assertMatch(false, ctype:isxdigit(127))
	].

iscntrl_test_() ->
	[
	?_assertMatch(true, ctype:iscntrl(0)),
	?_assertMatch(true, ctype:iscntrl(1)),
	?_assertMatch(true, ctype:iscntrl(2)),
	?_assertMatch(true, ctype:iscntrl(3)),
	?_assertMatch(true, ctype:iscntrl($\b)),
	?_assertMatch(true, ctype:iscntrl($\t)),
	?_assertMatch(true, ctype:iscntrl($\n)),
	?_assertMatch(true, ctype:iscntrl($\r)),
	?_assertMatch(true, ctype:iscntrl(16#1B)),
	?_assertMatch(true, ctype:iscntrl(127)),
	?_assertMatch(false, ctype:iscntrl($ )),
	?_assertMatch(false, ctype:iscntrl($-)),
	?_assertMatch(false, ctype:iscntrl($1)),
	?_assertMatch(false, ctype:iscntrl($9)),
	?_assertMatch(false, ctype:iscntrl($A)),
	?_assertMatch(false, ctype:iscntrl($Z)),
	?_assertMatch(false, ctype:isalnum($_)),
	?_assertMatch(false, ctype:iscntrl($a)),
	?_assertMatch(false, ctype:iscntrl($z)),
	?_assertMatch(false, ctype:iscntrl($[)),
	?_assertMatch(false, ctype:iscntrl($]))
	].

isupper_test_() ->
	[
	?_assertMatch(false, ctype:isupper(0)),
	?_assertMatch(false, ctype:isupper(1)),
	?_assertMatch(false, ctype:isupper(2)),
	?_assertMatch(false, ctype:isupper(3)),
	?_assertMatch(false, ctype:isupper($\b)),
	?_assertMatch(false, ctype:isupper($\t)),
	?_assertMatch(false, ctype:isupper($\n)),
	?_assertMatch(false, ctype:isupper($\r)),
	?_assertMatch(false, ctype:isupper(16#1B)),
	?_assertMatch(false, ctype:isupper(127)),
	?_assertMatch(false, ctype:isupper($ )),
	?_assertMatch(false, ctype:isupper($-)),
	?_assertMatch(false, ctype:isupper($1)),
	?_assertMatch(false, ctype:isupper($9)),
	?_assertMatch(false, ctype:isupper($@)),
	?_assertMatch(true, ctype:isupper($A)),
	?_assertMatch(true, ctype:isupper($Z)),
	?_assertMatch(false, ctype:isalnum($_)),
	?_assertMatch(false, ctype:isupper($a)),
	?_assertMatch(false, ctype:isupper($z)),
	?_assertMatch(false, ctype:isupper($[)),
	?_assertMatch(false, ctype:isupper($]))
	].

islower_test_() ->
	[
	?_assertMatch(false, ctype:islower(0)),
	?_assertMatch(false, ctype:islower(1)),
	?_assertMatch(false, ctype:islower(2)),
	?_assertMatch(false, ctype:islower(3)),
	?_assertMatch(false, ctype:islower($\b)),
	?_assertMatch(false, ctype:islower($\t)),
	?_assertMatch(false, ctype:islower($\n)),
	?_assertMatch(false, ctype:islower($\r)),
	?_assertMatch(false, ctype:islower(16#1B)),
	?_assertMatch(false, ctype:islower(127)),
	?_assertMatch(false, ctype:islower($-)),
	?_assertMatch(false, ctype:islower($ )),
	?_assertMatch(false, ctype:islower($1)),
	?_assertMatch(false, ctype:islower($9)),
	?_assertMatch(false, ctype:islower($@)),
	?_assertMatch(false, ctype:islower($A)),
	?_assertMatch(false, ctype:islower($Z)),
	?_assertMatch(false, ctype:isalnum($_)),
	?_assertMatch(true, ctype:islower($a)),
	?_assertMatch(true, ctype:islower($z)),
	?_assertMatch(false, ctype:islower($[)),
	?_assertMatch(false, ctype:islower($]))
	].

isalpha_test_() ->
	[
	?_assertMatch(false, ctype:isalpha(0)),
	?_assertMatch(false, ctype:isalpha(1)),
	?_assertMatch(false, ctype:isalpha(2)),
	?_assertMatch(false, ctype:isalpha(3)),
	?_assertMatch(false, ctype:isalpha($\b)),
	?_assertMatch(false, ctype:isalpha($\t)),
	?_assertMatch(false, ctype:isalpha($\n)),
	?_assertMatch(false, ctype:isalpha($\r)),
	?_assertMatch(false, ctype:isalpha(16#1B)),
	?_assertMatch(false, ctype:isalpha(127)),
	?_assertMatch(false, ctype:isalpha($-)),
	?_assertMatch(false, ctype:isalpha($ )),
	?_assertMatch(false, ctype:isalpha($1)),
	?_assertMatch(false, ctype:isalpha($9)),
	?_assertMatch(false, ctype:isalpha($@)),
	?_assertMatch(true, ctype:isalpha($A)),
	?_assertMatch(true, ctype:isalpha($Z)),
	?_assertMatch(false, ctype:isalnum($_)),
	?_assertMatch(true, ctype:isalpha($a)),
	?_assertMatch(true, ctype:isalpha($z)),
	?_assertMatch(false, ctype:isalpha($[)),
	?_assertMatch(false, ctype:isalpha($]))
	].

isalnum_test_() ->
	[
	?_assertMatch(false, ctype:isalnum(0)),
	?_assertMatch(false, ctype:isalnum(1)),
	?_assertMatch(false, ctype:isalnum(2)),
	?_assertMatch(false, ctype:isalnum(3)),
	?_assertMatch(false, ctype:isalnum($\b)),
	?_assertMatch(false, ctype:isalnum($\t)),
	?_assertMatch(false, ctype:isalnum($\n)),
	?_assertMatch(false, ctype:isalnum($\r)),
	?_assertMatch(false, ctype:isalnum(16#1B)),
	?_assertMatch(false, ctype:isalnum(127)),
	?_assertMatch(false, ctype:isalnum($-)),
	?_assertMatch(false, ctype:isalnum($ )),
	?_assertMatch(true, ctype:isalnum($1)),
	?_assertMatch(true, ctype:isalnum($9)),
	?_assertMatch(false, ctype:isalnum($@)),
	?_assertMatch(true, ctype:isalnum($A)),
	?_assertMatch(true, ctype:isalnum($Z)),
	?_assertMatch(false, ctype:isalnum($_)),
	?_assertMatch(true, ctype:isalnum($a)),
	?_assertMatch(true, ctype:isalnum($z)),
	?_assertMatch(false, ctype:isalnum($[)),
	?_assertMatch(false, ctype:isalnum($]))
	].

ispunct_test_() ->
	[
	?_assertMatch(false, ctype:ispunct(0)),
	?_assertMatch(false, ctype:ispunct(1)),
	?_assertMatch(false, ctype:ispunct(2)),
	?_assertMatch(false, ctype:ispunct(3)),
	?_assertMatch(false, ctype:ispunct($\b)),
	?_assertMatch(false, ctype:ispunct($\t)),
	?_assertMatch(false, ctype:ispunct($\n)),
	?_assertMatch(false, ctype:ispunct($\r)),
	?_assertMatch(false, ctype:ispunct(16#1B)),
	?_assertMatch(false, ctype:ispunct(127)),
	?_assertMatch(true, ctype:ispunct($-)),
	?_assertMatch(true, ctype:ispunct($.)),
	?_assertMatch(false, ctype:ispunct($ )),
	?_assertMatch(false, ctype:ispunct($1)),
	?_assertMatch(false, ctype:ispunct($9)),
	?_assertMatch(true, ctype:ispunct($@)),
	?_assertMatch(false, ctype:ispunct($A)),
	?_assertMatch(false, ctype:ispunct($Z)),
	?_assertMatch(true, ctype:ispunct($_)),
	?_assertMatch(false, ctype:ispunct($a)),
	?_assertMatch(false, ctype:ispunct($z)),
	?_assertMatch(true, ctype:ispunct($[)),
	?_assertMatch(true, ctype:ispunct($]))
	].

tolower_test_() ->
	[
	?_assertMatch(0, ctype:tolower(0)),
	?_assertMatch(1, ctype:tolower(1)),
	?_assertMatch(2, ctype:tolower(2)),
	?_assertMatch($\b, ctype:tolower($\b)),
	?_assertMatch($\t, ctype:tolower($\t)),
	?_assertMatch($\n, ctype:tolower($\n)),
	?_assertMatch($\r, ctype:tolower($\r)),
	?_assertMatch(16#1B, ctype:tolower(16#1B)),
	?_assertMatch(127, ctype:tolower(127)),
	?_assertMatch($ , ctype:tolower($ )),
	?_assertMatch($-, ctype:tolower($-)),
	?_assertMatch($., ctype:tolower($.)),
	?_assertMatch($1, ctype:tolower($1)),
	?_assertMatch($9, ctype:tolower($9)),
	?_assertMatch($@, ctype:tolower($@)),
	?_assertMatch($a, ctype:tolower($A)),
	?_assertMatch($z, ctype:tolower($Z)),
	?_assertMatch($_, ctype:tolower($_)),
	?_assertMatch($a, ctype:tolower($a)),
	?_assertMatch($z, ctype:tolower($z)),
	?_assertMatch($[, ctype:tolower($[)),
	?_assertMatch($], ctype:tolower($]))
	].

toupper_test_() ->
	[
	?_assertMatch(0, ctype:toupper(0)),
	?_assertMatch(1, ctype:toupper(1)),
	?_assertMatch(2, ctype:toupper(2)),
	?_assertMatch($\b, ctype:toupper($\b)),
	?_assertMatch($\t, ctype:toupper($\t)),
	?_assertMatch($\n, ctype:toupper($\n)),
	?_assertMatch($\r, ctype:toupper($\r)),
	?_assertMatch(16#1B, ctype:toupper(16#1B)),
	?_assertMatch(127, ctype:toupper(127)),
	?_assertMatch($ , ctype:toupper($ )),
	?_assertMatch($-, ctype:toupper($-)),
	?_assertMatch($., ctype:toupper($.)),
	?_assertMatch($1, ctype:toupper($1)),
	?_assertMatch($9, ctype:toupper($9)),
	?_assertMatch($@, ctype:toupper($@)),
	?_assertMatch($A, ctype:toupper($A)),
	?_assertMatch($Z, ctype:toupper($Z)),
	?_assertMatch($_, ctype:toupper($_)),
	?_assertMatch($A, ctype:toupper($a)),
	?_assertMatch($Z, ctype:toupper($z)),
	?_assertMatch($[, ctype:toupper($[)),
	?_assertMatch($], ctype:toupper($]))
	].

isbase_test_() ->
	[
	?_assertMatch(false, ctype:isbase($0, -1)),
	?_assertMatch(false, ctype:isbase($Z, 37)),
	?_assertMatch(false, ctype:isbase($., 2)),
	?_assertMatch(true, ctype:isbase($0, 2)),
	?_assertMatch(true, ctype:isbase($1, 2)),
	?_assertMatch(false, ctype:isbase($2, 2)),
	?_assertMatch(false, ctype:isbase($., 8)),
	?_assertMatch(true, ctype:isbase($0, 8)),
	?_assertMatch(true, ctype:isbase($1, 8)),
	?_assertMatch(true, ctype:isbase($2, 8)),
	?_assertMatch(true, ctype:isbase($3, 8)),
	?_assertMatch(true, ctype:isbase($4, 8)),
	?_assertMatch(true, ctype:isbase($5, 8)),
	?_assertMatch(true, ctype:isbase($6, 8)),
	?_assertMatch(true, ctype:isbase($7, 8)),
	?_assertMatch(false, ctype:isbase($8, 8)),
	?_assertMatch(false, ctype:isbase($., 10)),
	?_assertMatch(true, ctype:isbase($0, 10)),
	?_assertMatch(true, ctype:isbase($9, 10)),
	?_assertMatch(false, ctype:isbase($A, 10)),
	?_assertMatch(false, ctype:isbase($a, 10)),
	?_assertMatch(false, ctype:isbase($., 16)),
	?_assertMatch(true, ctype:isbase($0, 16)),
	?_assertMatch(true, ctype:isbase($9, 16)),
	?_assertMatch(true, ctype:isbase($A, 16)),
	?_assertMatch(true, ctype:isbase($a, 16)),
	?_assertMatch(true, ctype:isbase($F, 16)),
	?_assertMatch(true, ctype:isbase($f, 16)),
	?_assertMatch(false, ctype:isbase($G, 16)),
	?_assertMatch(false, ctype:isbase($g, 16)),
	?_assertMatch(true, ctype:isbase($0, 36)),
	?_assertMatch(true, ctype:isbase($Z, 36)),
	?_assertMatch(true, ctype:isbase($z, 36)),
	?_assertMatch(false, ctype:isbase($[, 36))
	].
