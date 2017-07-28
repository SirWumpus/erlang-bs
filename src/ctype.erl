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

-spec isblank(integer()) -> boolean().
isblank(Ch) ->
	Ch =:= 32 orelse Ch =:= ?TAB.

-spec isspace(integer()) -> boolean().
isspace(Ch) ->
	Ch =:= 32 orelse (?TAB =< Ch andalso Ch =< ?CR).

-spec isprint(integer()) -> boolean().
isprint(Ch) ->
	?SPC =< Ch andalso Ch < ?DEL.

-spec isdigit(integer()) -> boolean().
isdigit(Ch) ->
	$0 =< Ch andalso Ch =< $9.

-spec isxdigit(integer()) -> boolean().
isxdigit(Ch) ->
	isdigit(Ch) orelse ($A =< Ch andalso Ch =< $F) orelse ($a =< Ch andalso Ch =< $f).

-spec iscntrl(integer()) -> boolean().
iscntrl(Ch) ->
	(0 =< Ch andalso Ch < ?SPC) orelse Ch =:= ?DEL.

-spec isupper(integer()) -> boolean().
isupper(Ch) ->
	$A =< Ch andalso Ch =< $Z.

-spec islower(integer()) -> boolean().
islower(Ch) ->
	$a =< Ch andalso Ch =< $z.

-spec isalpha(integer()) -> boolean().
isalpha(Ch) ->
	isupper(Ch) orelse islower(Ch).

-spec isalnum(integer()) -> boolean().
isalnum(Ch) ->
	isalpha(Ch) orelse isdigit(Ch).

-spec ispunct(integer()) -> boolean().
ispunct(Ch) ->
	?SPC < Ch andalso Ch < ?DEL andalso not isalnum(Ch).

-spec tolower(integer()) -> integer().
tolower(Ch) ->
	case isupper(Ch) of
	true ->
		Ch + 16#20;
	false ->
		Ch
	end.

-spec toupper(integer()) -> integer().
toupper(Ch) ->
	case islower(Ch) of
	true ->
		Ch - 16#20;
	false ->
		Ch
	end.

-spec isbase(integer(), integer()) -> boolean().
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
