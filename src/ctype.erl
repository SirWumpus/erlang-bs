-module(ctype).
-export([
	isblank/1, isspace/1, isprint/1, isdigit/1, isxdigit/1,
        iscntrl/1, isalpha/1, isalnum/1, isupper/1, islower/1, ispunct/1
]).


isblank(Ch) ->
	Ch =:= 32 orelse Ch =:= 9.

isspace(Ch) ->
	Ch =:= 32 orelse (9 =< Ch andalso Ch =< 13).

isprint(Ch) ->
	32 =< Ch andalso Ch < 127.

isdigit(Ch) ->
	$0 =< Ch andalso Ch =< $9.

isxdigit(Ch) ->
	isdigit(Ch) orelse ($A =< Ch andalso Ch =< $F) orelse ($a =< Ch andalso Ch =< $f).

iscntrl(Ch) ->
	(0 =< Ch andalso Ch < 32) orelse Ch =:= 127.

isupper(Ch) ->
	$A =< Ch andalso Ch =< $Z.

islower(Ch) ->
	$a =< Ch andalso Ch =< $z.

isalpha(Ch) ->
	isupper(Ch) orelse islower(Ch).

isalnum(Ch) ->
	isalpha(Ch) orelse isdigit(Ch).

ispunct(Ch) ->
	32 < Ch andalso Ch < 127 andalso not isalnum(Ch).
