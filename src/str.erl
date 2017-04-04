-module(str).
-compile({no_auto_import,[error/1]}).
-export([
	at/2, cat/2, ncat/3, cmp/2, ncmp/3, cpy/1, ncpy/2, chr/2, rchr/2,
	error/1, len/1, rev/1, ltrim/1, rtrim/1, trim/1, spn/2, cspn/2, sub/2,
	sub/3, tok/2, casecmp/2, ncasecmp/3, lower/1, upper/1, tr/2, tr/3,
	ftime/2, lpad/3, rpad/3, pad_int/3, pad_sign_int/3, to_int/2
]).

%% These will move to another module (eventually).
-export([time_to_epoch_seconds/1, time_zone_seconds/0]).


len(Bs) ->
	byte_size(Bs).

at(Bs, Index) when Index < 0 orelse byte_size(Bs) =< Index ->
	badarg;
at(<<Ch:8, _/binary>>, 0) ->
	Ch;
at(<<_:8, Rest/binary>>, Index) ->
	at(Rest, Index-1).

cat(<<Bs1/binary>>, <<Bs2/binary>>) ->
	<<Bs1/binary, Bs2/binary>>;
cat(Str1, Str2) when is_list(Str1) andalso is_list(Str2) ->
	Bs1 = list_to_binary(Str1),
	Bs2 = list_to_binary(Str2),
	<<Bs1/binary, Bs2/binary>>.

ncat(_, _, Length) when Length < 0 ->
	badarg;
ncat(<<Bs1/binary>>, _, 0) ->
	Bs1;
ncat(<<Bs1/binary>>, <<Bs2/binary>>, Length) when byte_size(Bs2) < Length ->
	ncat(Bs1, Bs2, byte_size(Bs2));
ncat(<<Bs1/binary>>, <<Bs2/binary>>, Length) ->
	<<Bs1/binary, Bs2:Length/binary>>.

chr(<<Bs/binary>>, Ch) ->
	chr(Bs, Ch, 0).
chr(<<>>, _Ch, _Index) ->
	-1;
chr(<<Ch:8, _/binary>>, Ch, Index) ->
	Index;
chr(<<_:8, Rest/binary>>, Ch, Index) ->
	chr(Rest, Ch, Index+1).

rchr(<<Bs/binary>>, Ch) ->
	rchr(Bs, Ch, 0, -1).
rchr(<<>>, _, _, Last) ->
	Last;
rchr(<<Ch:8, Rest/binary>>, Ch, Index, _) ->
	rchr(Rest, Ch, Index+1, Index);
rchr(<<_:8, Rest/binary>>, Ch, Index, Last) ->
	rchr(Rest, Ch, Index+1, Last).

rev(<<Bs/binary>>) ->
	rev(Bs, <<>>).
rev(<<>>, Acc) ->
	Acc;
rev(<<Ch:8, Rest/binary>>, Acc) ->
	rev(Rest, <<Ch, Acc/binary>>).

ltrim(<<>>) ->
	<<>>;
ltrim(<<Ch:8, Rest/binary>>) when Ch == 32 orelse (9 =< Ch andalso Ch =< 13) ->
	ltrim(Rest);
ltrim(Rest) ->
	Rest.

rtrim(<<>>) ->
	<<>>;
rtrim(<<Bs/binary>>) ->
	rev(ltrim(rev(Bs))).

trim(<<Bs/binary>>) ->
	ltrim(rtrim(Bs)).

spn(<<Bs/binary>>, Delims) ->
	spn(Bs, Delims, 0).
spn(<<>>, _Delims, Span) ->
	Span;
spn(<<Ch:8, Rest/binary>>, Delims, Span) ->
	case chr(Delims, Ch) of
	-1 ->
		Span;
	_ ->
		spn(Rest, Delims, Span+1)
	end.

cspn(<<Bs/binary>>, Delims) ->
	cspn(Bs, Delims, 0).
cspn(<<>>, _Delims, Span) ->
	Span;
cspn(<<Ch:8, Rest/binary>>, Delims, Span) ->
	case chr(Delims, Ch) of
	-1 ->
		cspn(Rest, Delims, Span+1);
	_ ->
		Span
	end.

sub(Bs, Start) ->
	sub(Bs, Start, len(Bs)).
sub(_Bs, Start, Stop) when Stop =< Start ->
	<<>>;
sub(Bs, Start, Stop) ->
	sub(Bs, Start, Stop, <<>>).
sub(_Bs, 0, 0, Acc) ->
	Acc;
sub(<<>>, _Start, _Stop, Acc) ->
	Acc;
sub(<<Ch:8, Rest/binary>>, 0, Stop, Acc) ->
	sub(Rest, 0, Stop-1, <<Acc/binary, Ch>>);
sub(<<_:8, Rest/binary>>, Start, Stop, Acc) ->
	sub(Rest, Start-1, Stop-1, Acc).

tok(Bs, Delims) ->
	% Skip leading delimiters.
	SepLen = spn(Bs, Delims),
	<<_:SepLen/binary, Rest/binary>> = Bs,

	TokLen = cspn(Rest, Delims),
	<<Token:TokLen/binary, Rest2/binary>> = Rest,

	% Consume trailing delimiters.
	SepLen2 = spn(Rest2, Delims),
	<<_:SepLen2/binary, Rest3/binary>> = Rest2,
	{Token, Rest3}.

ncmp(_A, _B, 0) ->
	0;
ncmp(<<>>, <<>>, _Length) ->
	0;
ncmp(<<_Ach:8, _/binary>>, <<>>, _Length) ->
	1;
ncmp(<<>>, <<_Bch:8, _/binary>>, _Length) ->
	-1;
ncmp(<<Ach:8, A/binary>>, <<Bch:8, B/binary>>, Length) ->
	if
		Ach =:= Bch ->
			ncmp(A, B, Length-1);
		Ach < Bch ->
			-1;
		Ach > Bch ->
			1
	end.

cmp(A, B) ->
	ncmp(A, B, max(byte_size(A), byte_size(B))).

ncasecmp(_A, _B, 0) ->
	0;
ncasecmp(<<>>, <<>>, _Length) ->
	0;
ncasecmp(<<_Ach:8, _/binary>>, <<>>, _Length) ->
	1;
ncasecmp(<<>>, <<_Bch:8, _/binary>>, _Length) ->
	-1;
ncasecmp(<<Ach:8, A/binary>>, <<Bch:8, B/binary>>, Length) ->
	UpperA = ctype:toupper(Ach),
	UpperB = ctype:toupper(Bch),
	if
		UpperA =:= UpperB ->
			ncasecmp(A, B, Length-1);
		UpperA < UpperB ->
			-1;
		UpperA > UpperB ->
			1
	end.

casecmp(A, B) ->
	ncasecmp(A, B, max(byte_size(A), byte_size(B))).

cpy(Bs) ->
	binary:copy(Bs).

ncpy(Bs, Length) ->
	ncpy(Bs, Length, <<>>).
ncpy(_, 0, Acc) ->
	Acc;
ncpy(<<>>, _, Acc) ->
	Acc;
ncpy(<<Ch:8, Rest/binary>>, Length, Acc) ->
	ncpy(Rest, Length-1, <<Acc/binary, Ch:8>>).

upper(Bs) ->
	upper(Bs, <<>>).
upper(<<>>, Acc) ->
	Acc;
upper(<<Octet:8, Rest/binary>>, Acc) ->
	upper(Rest, <<Acc/binary, (ctype:toupper(Octet)):8>>).

lower(Bs) ->
	lower(Bs, <<>>).
lower(<<>>, Acc) ->
	Acc;
lower(<<Octet:8, Rest/binary>>, Acc) ->
	lower(Rest, <<Acc/binary, (ctype:tolower(Octet)):8>>).


error(Reason) ->
	%% Taken from NetBSD 7.1 man error; assumes Erlang uses errno names.
	case Reason of
	eperm -> <<"Operation not permitted.">>;
	enoent -> <<"No such file or directory.">>;
	esrch -> <<"No such process.">>;
	eintr -> <<"Interrupted function call.">>;
	eio -> <<"Input/output error.">>;
	enxio -> <<"Device not configured.">>;
	enoexec -> <<"Exec format error.">>;
	ebadf -> <<"Bad file descriptor.">>;
	echild -> <<"No child processes.">>;
	edeadlk -> <<"Resource deadlock avoided.">>;
	enomem -> <<"Cannot allocate memory.">>;
	eacces -> <<"Permission denied.">>;
	efault -> <<"Bad address.">>;
	enotblk -> <<"Block device required.">>;
	ebusy -> <<"Resource busy.">>;
	eexist -> <<"File exists.">>;
	exdev -> <<"Improper link.">>;
	enodev -> <<"Operation not supported by device.">>;
	enotdir -> <<"Not a directory.">>;
	eisdir -> <<"Is a directory.">>;
	einval -> <<"Invalid argument.">>;
	enfile -> <<"Too many open files in system.">>;
	emfile -> <<"Too many open files.">>;
	enotty -> <<"Inappropriate ioctl for device.">>;
	etxtbsy -> <<"Text file busy.">>;
	efbig -> <<"File too large.">>;
	enospc -> <<"Device out of space.">>;
	espipe -> <<"Illegal seek.">>;
	erofs -> <<"Read-only file system.">>;
	emlink -> <<"Too many links.">>;
	epipe -> <<"Broken pipe.">>;
	edom -> <<"Numerical argument out of domain.">>;
	erange -> <<"Result too large or too small.">>;
	eagain -> <<"Resource temporarily unavailable.">>;
	einprogress -> <<"Operation now in progress.">>;
	ealready -> <<"Operation already in progress.">>;
	enotsock -> <<"Socket operation on non-socket.">>;
	edestaddrreq -> <<"Destination address required.">>;
	emsgsize -> <<"Message too long.">>;
	eprototype -> <<"Protocol wrong type for socket.">>;
	enoprotoopt -> <<"Protocol option not available.">>;
	eprotonosupport -> <<"Protocol not supported.">>;
	esocktnosupport -> <<"Socket type not supported.">>;
	eopnotsupp -> <<"Operation not supported.">>;
	epfnosupport -> <<"Protocol family not supported.">>;
	eafnosupport -> <<"Address family not supported by protocol family.">>;
	eaddrinuse -> <<"Address already in use.">>;
	eaddrnotavail -> <<"Cannot assign requested address.">>;
	enetdown -> <<"Network is down.">>;
	enetunreach -> <<"Network is unreachable.">>;
	enetreset -> <<"Network dropped connection on reset.">>;
	econnaborted -> <<"Software caused connection abort.">>;
	econnreset -> <<"Connection reset by peer.">>;
	enobufs -> <<"No buffer space available.">>;
	eisconn -> <<"Socket is already connected.">>;
	enotconn -> <<"Socket is not connected.">>;
	eshutdown -> <<"Cannot send after socket shutdown.">>;
	etoomanyrefs -> <<"Too many references: can't splice.">>;
	etimedout -> <<"Operation timed out.">>;
	econnrefused -> <<"Connection refused.">>;
	eloop -> <<"Too many levels of symbolic links.">>;
	enametoolong -> <<"File name too long.">>;
	ehostdown -> <<"Host is down.">>;
	ehostunreach -> <<"No route to host.">>;
	enotempty -> <<"Directory not empty.">>;
	eproclim -> <<"Too many processes.">>;
	eusers -> <<"Too many users.">>;
	edquot -> <<"Disc quota exceeded.">>;
	estale -> <<"Stale NFS file handle.">>;
	eremote -> <<"Too many levels of remote in path.">>;
	ebadrpc -> <<"RPC struct is bad.">>;
	erpcmismatch -> <<"RPC version wrong.">>;
	eprogunavail -> <<"RPC prog.">>;
	eprogmismatch -> <<"Program version wrong.">>;
	eprocunavail -> <<"Bad procedure for program.">>;
	enolck -> <<"No locks available.">>;
	enosys -> <<"Function not implemented.">>;
	eftype -> <<"Inappropriate file type or format.">>;
	eauth -> <<"Authentication error.">>;
	eneedauth -> <<"Need authenticator.">>;
	eidrm -> <<"Identifier removed.">>;
	enomsg -> <<"No message of the desired type.">>;
	eoverflow -> <<"Value too large to be stored in data type.">>;
	eilseq -> <<"Illegal byte sequence.">>;
	enotsup -> <<"Not supported.">>;
	ecanceled -> <<"Operation canceled.">>;
	ebadmsg -> <<"Bad or corrupt message.">>;
	enodata -> <<"No message available.">>;
	enosr -> <<"No STREAM resources.">>;
	enostr -> <<"Not a STREAM.">>;
	etime -> <<"STREAM ioctl timeout.">>;
	enoattr -> <<"Attribute not found.">>;
	emultihop -> <<"Multihop attempted.">>;
	enolink -> <<"Link has been severed.">>;
	eproto -> <<"Protocol error.">>;
	Reason -> atom_to_binary(Reason, utf8)
	end.

tr(Bs, FromSet) ->
	tr(Bs, FromSet, <<>>).
tr(Bs, <<>>, _ToSet) ->
	Bs;
tr(Bs, FromSet, ToSet) ->
	tr(Bs, FromSet, ToSet, <<>>).
tr(<<>>, _FromSet, _ToSet, Acc) ->
	Acc;
tr(<<Ch:8, Rest/binary>>, FromSet, ToSet, Acc) ->
	ToLen = byte_size(ToSet),
	case chr(FromSet, Ch) of
	-1 ->
		tr(Rest, FromSet, ToSet, <<Acc/binary, Ch:8>>);
	_Index when ToLen == 0 ->
		tr(Rest, FromSet, ToSet, Acc);
	Index when ToLen =< Index ->
		Last = binary:last(ToSet),
		tr(Rest, FromSet, ToSet, <<Acc/binary, Last:8>>);
	Index ->
		Replace = binary:at(ToSet, Index),
		tr(Rest, FromSet, ToSet, <<Acc/binary, Replace:8>>)
	end.

-define(WEEK_DAYS_FULL, <<"Monday">>,<<"Tuesday">>,<<"Wednesday">>,<<"Thursday">>,<<"Friday">>,<<"Saturday">>,<<"Sunday">>).
-define(WEEK_DAYS_SHORT, <<"Mon">>,<<"Tue">>,<<"Wed">>,<<"Thu">>,<<"Fri">>,<<"Sat">>,<<"Sun">>).
-define(MONTH_FULL, <<"January">>,<<"February">>,<<"March">>,<<"April">>,<<"May">>,<<"June">>,<<"July">>,<<"August">>,<<"September">>,<<"October">>,<<"November">>,<<"December">>).
-define(MONTH_SHORT, <<"Jan">>,<<"Feb">>,<<"Mar">>,<<"Apr">>,<<"May">>,<<"Jun">>,<<"Jul">>,<<"Aug">>,<<"Sep">>,<<"Oct">>,<<"Nov">>,<<"Dec">>).

ftime(Fmt, {Date, Time}) ->
	ftime(Fmt, {Date, Time, time_zone_seconds()});
ftime(Fmt, {Date, Time, Tz}) ->
	ftime(Fmt, {Date, Time, Tz}, <<>>).
ftime(<<>>, _DateTime, Acc) ->
	Acc;
ftime(<<"%", Ch:8, Rest/binary>>, {Date, Time, Tz}, Acc) ->
	{Year, Month, Day} = Date,
	{Hour, Min, Sec} = Time,

	NewAcc = case Ch of
	$A ->
		FullDay = element(calendar:day_of_the_week(Date), {?WEEK_DAYS_FULL}),
		<<Acc/binary, FullDay/binary>>;
	$a ->
		ShortDay = element(calendar:day_of_the_week(Date), {?WEEK_DAYS_SHORT}),
		<<Acc/binary, ShortDay/binary>>;
	$B ->
		FullMonth = element(Month, {?MONTH_FULL}),
		<<Acc/binary, FullMonth/binary>>;
	$b ->
		ShortMonth = element(Month, {?MONTH_SHORT}),
		<<Acc/binary, ShortMonth/binary>>;
	$h ->
		ShortMonth = element(Month, {?MONTH_SHORT}),
		<<Acc/binary, ShortMonth/binary>>;
	$C ->
		Century = pad_int(Year div 100, $0, 2),
		<<Acc/binary, Century/binary>>;
	$c ->
		Cdate = ftime(<<"%e %b %Y %H:%M:%S">>, {Date, Time, Tz}),
		<<Acc/binary, Cdate/binary>>;
	$D ->
		UsDate = ftime(<<"%m/%d/%y">>, {Date, Time, Tz}),
		<<Acc/binary, UsDate/binary>>;
	$d ->
		Dday = pad_int(Day, $0, 2),
		<<Acc/binary, Dday/binary>>;
	$e ->
		Eday = pad_int(Day, $ , 2),
		<<Acc/binary, Eday/binary>>;
	$F ->
		IsoDate = ftime(<<"%Y-%m-%d">>, {Date, Time, Tz}),
		<<Acc/binary, IsoDate/binary>>;
	$G ->
		throw({error, not_supported});
	$g ->
		throw({error, not_supported});
	$H ->
		Hr = pad_int(Hour, $0, 2),
		<<Acc/binary, Hr/binary>>;
	$I ->
		Hr12 = pad_int(Hour rem 12, $0, 2),
		<<Acc/binary, Hr12/binary>>;
	$j ->
		Yday = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Year, 1, 1) + 1,
		DecDayOfYear = pad_int(Yday, $0, 3),
		<<Acc/binary, DecDayOfYear/binary>>;
	$k ->
		SpcHr = pad_int(Hour, $ , 2),
		<<Acc/binary, SpcHr/binary>>;
	$l ->
		SpcHr12 = pad_int(Hour rem 12, $ , 2),
		<<Acc/binary, SpcHr12/binary>>;
	$M ->
		Mn = pad_int(Min, $0, 2),
		<<Acc/binary, Mn/binary>>;
	$m ->
		Mon = pad_int(Month, $0, 2),
		<<Acc/binary, Mon/binary>>;
	$n ->
		<<Acc/binary, $\n>>;
	$p ->
		if
		Hour < 12 ->
			<<Acc/binary, "am">>;
		Hour >= 12 ->
			<<Acc/binary, "pm">>
		end;
	$R ->
		HrMn = ftime(<<"%H:%M">>, {Date, Time, Tz}),
		<<Acc/binary, HrMn/binary>>;
	$r ->
		HrMn12 = ftime(<<"%l:%M %p">>, {Date, Time, Tz}),
		<<Acc/binary, HrMn12/binary>>;
	$S ->
		Seconds = pad_int(Sec, $0, 2),
		<<Acc/binary, Seconds/binary>>;
	$s ->
		Epoch = integer_to_binary(time_to_epoch_seconds({Date, Time})),
		<<Acc/binary, Epoch/binary>>;
	$T ->
		IsoTime = ftime(<<"%H:%M:%S">>, {Date, Time, Tz}),
		<<Acc/binary, IsoTime/binary>>;
	$t ->
		<<Acc/binary, $\t>>;
	$U ->
		throw({error, not_supported});
	$u ->
		throw({error, not_supported});
	$V ->
		{_Year, Week} = calendar:iso_week_number(Date),
		WeekNum = pad_int(Week, $0, 2),
		<<Acc/binary, WeekNum/binary>>;
	$v ->
		Vdate = ftime(<<"%e-%b-%Y">>, {Date, Time, Tz}),
		<<Acc/binary, Vdate/binary>>;
	$W ->
		throw({error, not_supported});
	$w ->
		throw({error, not_supported});
	$X ->
		throw({error, not_supported});
	$x ->
		throw({error, not_supported});
	$Y ->
		FullYr = pad_int(Year, $0, 4),
		<<Acc/binary, FullYr/binary>>;
	$y ->
		ShortYr = pad_int(Year rem 100, $0, 2),
		<<Acc/binary, ShortYr/binary>>;
	$Z ->
		throw({error, not_supported});
	$z ->
		TzHr = pad_sign_int(Tz div 3600, $0, 3),
		TzMn = pad_int(abs((Tz rem 3600) div 60), $0, 2),
		<<Acc/binary, TzHr/binary, TzMn/binary>>;
	$% ->
		<<Acc/binary, $%>>;
	_ ->
		throw({error, einval})
	end,
	ftime(Rest, {Date, Time, Tz}, NewAcc);
ftime(<<Ch:8, Rest/binary>>, DateTime, Acc) ->
	ftime(Rest, DateTime, <<Acc/binary, Ch:8>>).

lpad(Bs, _Pad, Width) when Width =< byte_size(Bs) ->
	Bs;
lpad(Bs, Pad, Width) ->
	lpad(<<Pad:8, Bs/binary>>, Pad, Width).

rpad(Bs, _Pad, Width) when Width =< byte_size(Bs) ->
	Bs;
rpad(Bs, Pad, Width) ->
	rpad(<<Bs/binary, Pad:8>>, Pad, Width).

pad_int(Int, $0, Width) when Int < 0 ->
	Num = pad_int(-Int, $0, Width-1),
	<<$-, Num/binary>>;
pad_int(Int, Pad, Width) ->
	lpad(integer_to_binary(Int), Pad, Width).

pad_sign_int(Int, $0, Width) when Int < 0 ->
	Num = pad_int(-Int, $0, Width-1),
	<<$-, Num/binary>>;
pad_sign_int(Int, $0, Width) ->
	Num = pad_int(Int, $0, Width-1),
	<<$+, Num/binary>>;
pad_sign_int(Int, Pad, Width) when Int < 0 ->
	Num = integer_to_binary(-Int),
	lpad(<<$-, Num/binary>>, Pad, Width);
pad_sign_int(Int, Pad, Width) ->
	Num = integer_to_binary(Int),
	lpad(<<$+, Num/binary>>, Pad, Width).

to_int(<<"0x", Rest/binary>>, Base) when Base == 0 orelse Base == 16 ->
	to_int(Rest, 16, 0, 1, false);
to_int(<<"0", Rest/binary>>, 0) ->
	to_int(Rest, 8, 0, 1, false);
to_int(Bs, 0) ->
	to_int(Bs, 10);
to_int(<<$ , Rest/binary>>, Base) ->
	to_int(Rest, Base);
to_int(<<$-, Rest/binary>>, 10) ->
	to_int(Rest, 10, 0, -1, false);
to_int(<<$+, Rest/binary>>, 10) ->
	to_int(Rest, 10, 0, 1, false);
to_int(Bs, Base) ->
	to_int(Bs, Base, 0, 1, false).
to_int(<<>>, _Base, _Acc, _Sign, false) ->
	badarg;
to_int(<<>>, _Base, Acc, Sign, true) ->
	{ Sign * Acc, <<>> };
to_int(<<Ch:8, Rest/binary>>, Base, Acc, Sign, Has_digits) ->
	case ctype:isbase(Ch, Base) of
	true ->
		case ctype:isdigit(Ch) of
		true ->
			to_int(Rest, Base, Acc * Base + (Ch - $0), Sign, true);
		false ->
			to_int(Rest, Base, Acc * Base + (10 + ctype:toupper(Ch) - $A), Sign, true)
		end;
	false when Has_digits ->
		{ Sign * Acc, <<Ch:8, Rest/binary>> };
	false ->
		badarg
	end.

%%
%% These will move to another module (eventually).
%%
time_to_epoch_seconds({Date = {Year, _Month, _Day}, {Hour, Min, Sec}}) ->
	Yday = calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days(Year, 1, 1),
	Sec + Min * 60 + Hour * 3600 + Yday * 86400 + (Year - 1970) * 31536000 + ((Year - 1969) div 4) * 86400.

time_zone_seconds() ->
	Local = erlang:localtime(),
	Utc = erlang:localtime_to_universaltime(Local),
	time_to_epoch_seconds(Local) - time_to_epoch_seconds(Utc).

