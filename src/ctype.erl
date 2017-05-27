-module(ctype).
-export([
	isblank/1, isspace/1, isprint/1, isdigit/1, isxdigit/1,
        iscntrl/1, isalpha/1, isalnum/1, isupper/1, islower/1, ispunct/1,
        tolower/1, toupper/1, isbase/2
]).

%%
%% These functions assume ASCII.
%%

-define(BEL, 7).
-define(BS,  8).
-define(TAB, 9).
-define(LF,  10).
-define(VT,  11).
-define(FF,  12).
-define(CR,  13).
-define(ESC, 27).
-define(SPC, 32).
-define(DEL, 127).

isblank(Ch) ->
	Ch =:= 32 orelse Ch =:= ?TAB.

isspace(Ch) ->
	Ch =:= 32 orelse (?TAB =< Ch andalso Ch =< ?CR).

isprint(Ch) ->
	?SPC =< Ch andalso Ch < ?DEL.

isdigit(Ch) ->
	$0 =< Ch andalso Ch =< $9.

isxdigit(Ch) ->
	isdigit(Ch) orelse ($A =< Ch andalso Ch =< $F) orelse ($a =< Ch andalso Ch =< $f).

iscntrl(Ch) ->
	(0 =< Ch andalso Ch < ?SPC) orelse Ch =:= ?DEL.

isupper(Ch) ->
	$A =< Ch andalso Ch =< $Z.

islower(Ch) ->
	$a =< Ch andalso Ch =< $z.

isalpha(Ch) ->
	isupper(Ch) orelse islower(Ch).

isalnum(Ch) ->
	isalpha(Ch) orelse isdigit(Ch).

ispunct(Ch) ->
	?SPC < Ch andalso Ch < ?DEL andalso not isalnum(Ch).

tolower(Ch) ->
	case isupper(Ch) of
	true ->
		Ch + 16#20;
	false ->
		Ch
	end.

toupper(Ch) ->
	case islower(Ch) of
	true ->
		Ch - 16#20;
	false ->
		Ch
	end.

isbase(Ch, Base) when 2 =< Base andalso Base =< 36 ->
	case str:chr(<<"0123456789abcdefghijklmnopqrstuvwxyz">>, tolower(Ch)) of
	-1 ->
		false;
	Index when Index < Base ->
		true;
	_ ->
		false
	end;
isbase(_Ch, _Base) ->
	false.
