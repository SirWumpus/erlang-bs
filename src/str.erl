-module(str).
-compile({no_auto_import,[error/1]}).
-export([
	at/2, cat/2, ncat/3, cmp/2, ncmp/3, cpy/1, ncpy/2, chr/2, rchr/2,
	error/1, len/1, rev/1, ltrim/1, rtrim/1, trim/1, spn/2, cspn/2, sub/2,
	sub/3, tok/2, casecmp/2, ncasecmp/3, lower/1, upper/1, tr/2, tr/3,
	ftime/2, lpad/3, rpad/3, pad_int/3, pad_sign_int/3, to_int/2, to_int/3,
	ptime/2, to_date_time/1, str/2, casestr/2, isprintable/1,
	token/1, token/2, split/1, split/2
]).

-ifdef(EUNIT).
-export([
	iso_date_time/1, index_of_word/2
]).
-endif.

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
-define(LINESPACE, <<" \t">>).
-define(WHITESPACE, <<" \t\n\r\f\v">>).

-type base() :: 0..36.

-spec len(binary()) -> non_neg_integer().
len(Bs) ->
	byte_size(Bs).

-spec at(binary(), integer()) -> badarg | byte().
at(Bs, Index) when Index < 0 orelse byte_size(Bs) =< Index ->
	badarg;
at(Bs, Index) ->
	binary:at(Bs, Index).

% Classic method
%
%at(Bs, Index) when Index < 0 orelse byte_size(Bs) =< Index ->
%	badarg;
%at(<<Ch:8, _/binary>>, 0) ->
%	Ch;
%at(<<_:8, Rest/binary>>, Index) ->
%	at(Rest, Index-1).

-spec cat(binary() | string(), binary() | string()) -> binary().
cat(<<Bs1/binary>>, <<Bs2/binary>>) ->
	<<Bs1/binary, Bs2/binary>>;
cat(Str1, Str2) when is_list(Str1) andalso is_list(Str2) ->
	Bs1 = list_to_binary(Str1),
	Bs2 = list_to_binary(Str2),
	<<Bs1/binary, Bs2/binary>>.

-spec ncat(binary() | string(), binary() | string(), integer()) -> badarg | binary().
ncat(_, _, Length) when Length < 0 ->
	badarg;
ncat(<<Bs1/binary>>, _, 0) ->
	Bs1;
ncat(<<Bs1/binary>>, <<Bs2/binary>>, Length) when byte_size(Bs2) < Length ->
	ncat(Bs1, Bs2, byte_size(Bs2));
ncat(<<Bs1/binary>>, <<Bs2/binary>>, Length) ->
	<<Bs1/binary, Bs2:Length/binary>>.

-spec chr(binary(), integer()) -> -1 | non_neg_integer().
chr(<<Bs/binary>>, Ch) ->
	chr(Bs, Ch, 0).

-spec chr(binary(), integer(), integer()) -> -1 | non_neg_integer().
chr(<<>>, _Ch, _Index) ->
	-1;
chr(<<Ch:8, _/binary>>, Ch, Index) ->
	Index;
chr(<<_:8, Rest/binary>>, Ch, Index) ->
	chr(Rest, Ch, Index+1).

-spec rchr(binary(), integer()) -> -1 | non_neg_integer().
rchr(<<Bs/binary>>, Ch) ->
	rchr(Bs, Ch, 0, -1).

-spec rchr(binary(), integer(), integer(), integer()) -> -1 | non_neg_integer().
rchr(<<>>, _, _, Last) ->
	Last;
rchr(<<Ch:8, Rest/binary>>, Ch, Index, _) ->
	rchr(Rest, Ch, Index+1, Index);
rchr(<<_:8, Rest/binary>>, Ch, Index, Last) ->
	rchr(Rest, Ch, Index+1, Last).

-spec rev(binary())-> binary().
rev(<<Bs/binary>>) ->
	rev(Bs, <<>>).

-spec rev(binary(), binary())-> binary().
rev(<<>>, Acc) ->
	Acc;
rev(<<Ch:8, Rest/binary>>, Acc) ->
	rev(Rest, <<Ch, Acc/binary>>).

-spec ltrim(binary()) -> binary().
ltrim(<<>>) ->
	<<>>;
ltrim(<<Ch:8, Rest/binary>>) when Ch == ?SPC orelse (?TAB =< Ch andalso Ch =< ?CR) ->
	ltrim(Rest);
ltrim(Rest) ->
	Rest.

-spec rtrim(binary()) -> binary().
rtrim(<<>>) ->
	<<>>;
rtrim(<<Bs/binary>>) ->
	rev(ltrim(rev(Bs))).

-spec trim(binary()) -> binary().
trim(<<Bs/binary>>) ->
	ltrim(rtrim(Bs)).

-spec spn(binary(), binary()) -> non_neg_integer().
spn(<<Bs/binary>>, Delims) ->
	spn(Bs, Delims, 0).

-spec spn(binary(), binary(), non_neg_integer()) -> non_neg_integer().
spn(<<>>, _Delims, Span) ->
	Span;
spn(<<Ch:8, Rest/binary>>, Delims, Span) ->
	case chr(Delims, Ch) of
	-1 ->
		Span;
	_ ->
		spn(Rest, Delims, Span+1)
	end.

-spec cspn(binary(), binary()) -> non_neg_integer().
cspn(<<Bs/binary>>, Delims) ->
	cspn(Bs, Delims, 0).

-spec cspn(binary(), binary(), non_neg_integer()) -> non_neg_integer().
cspn(<<>>, _Delims, Span) ->
	Span;
cspn(<<Ch:8, Rest/binary>>, Delims, Span) ->
	case chr(Delims, Ch) of
	-1 ->
		cspn(Rest, Delims, Span+1);
	_ ->
		Span
	end.

-spec sub(binary(), non_neg_integer()) -> binary().
sub(Bs, Start) ->
	sub(Bs, Start, byte_size(Bs)).

-spec sub(binary(), non_neg_integer(), non_neg_integer()) -> binary().
sub(_Bs, Start, Stop) when Stop =< Start ->
	<<>>;
sub(Bs, Start, Stop) ->
	binary_part(Bs, Start, Stop - Start).

% Classic method
%
%sub(_Bs, Start, Stop) when Stop =< Start ->
%	<<>>;
%sub(Bs, Start, Stop) ->
%	sub(Bs, Start, Stop, <<>>).
%sub(_Bs, 0, 0, Acc) ->
%	Acc;
%sub(<<>>, _Start, _Stop, Acc) ->
%	Acc;
%sub(<<Ch:8, Rest/binary>>, 0, Stop, Acc) ->
%	sub(Rest, 0, Stop-1, <<Acc/binary, Ch>>);
%sub(<<_:8, Rest/binary>>, Start, Stop, Acc) ->
%	sub(Rest, Start-1, Stop-1, Acc).

-spec tok(binary(), binary()) -> {binary(), binary()}.
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

-spec ncmp(binary(), binary(), non_neg_integer()) -> -1 | 0 | 1.
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

-spec cmp(binary(), binary()) ->  -1 | 0 | 1.
cmp(A, B) ->
	ncmp(A, B, max(byte_size(A), byte_size(B))).

-spec ncasecmp(binary(), binary(), non_neg_integer()) -> -1 | 0 | 1.
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

-spec casecmp(binary(), binary()) ->  -1 | 0 | 1.
casecmp(A, B) ->
	ncasecmp(A, B, max(byte_size(A), byte_size(B))).

-spec cpy(binary()) -> binary().
cpy(Bs) ->
	binary:copy(Bs).

-spec ncpy(binary(), non_neg_integer()) -> binary().
ncpy(Bs, Length) ->
	ncpy(Bs, Length, <<>>).

-spec ncpy(binary(), non_neg_integer(), binary()) -> binary().
ncpy(_, 0, Acc) ->
	Acc;
ncpy(<<>>, _, Acc) ->
	Acc;
ncpy(<<Ch:8, Rest/binary>>, Length, Acc) ->
	ncpy(Rest, Length-1, <<Acc/binary, Ch:8>>).

-spec upper(binary()) -> binary().
upper(Bs) ->
	<< << (ctype:toupper(Octet)) >> || <<Octet>> <= Bs >>.

-spec lower(binary()) -> binary().
lower(Bs) ->
	<< << (ctype:tolower(Octet)) >> || <<Octet>> <= Bs >>.

%% Taken from NetBSD 7.1 man error; assumes Erlang uses errno names.
-spec error(atom()) -> binary().
error(eperm)		-> <<"Operation not permitted.">>;
error(enoent)		-> <<"No such file or directory.">>;
error(esrch)		-> <<"No such process.">>;
error(eintr)		-> <<"Interrupted function call.">>;
error(eio)		-> <<"Input/output error.">>;
error(enxio)		-> <<"Device not configured.">>;
error(enoexec)		-> <<"Exec format error.">>;
error(ebadf)		-> <<"Bad file descriptor.">>;
error(echild)		-> <<"No child processes.">>;
error(edeadlk)		-> <<"Resource deadlock avoided.">>;
error(enomem)		-> <<"Cannot allocate memory.">>;
error(eacces)		-> <<"Permission denied.">>;
error(efault)		-> <<"Bad address.">>;
error(enotblk)		-> <<"Block device required.">>;
error(ebusy)		-> <<"Resource busy.">>;
error(eexist)		-> <<"File exists.">>;
error(exdev)		-> <<"Improper link.">>;
error(enodev)		-> <<"Operation not supported by device.">>;
error(enotdir)		-> <<"Not a directory.">>;
error(eisdir)		-> <<"Is a directory.">>;
error(einval)		-> <<"Invalid argument.">>;
error(enfile)		-> <<"Too many open files in system.">>;
error(emfile)		-> <<"Too many open files.">>;
error(enotty)		-> <<"Inappropriate ioctl for device.">>;
error(etxtbsy)		-> <<"Text file busy.">>;
error(efbig)		-> <<"File too large.">>;
error(enospc)		-> <<"Device out of space.">>;
error(espipe)		-> <<"Illegal seek.">>;
error(erofs)		-> <<"Read-only file system.">>;
error(emlink)		-> <<"Too many links.">>;
error(epipe)		-> <<"Broken pipe.">>;
error(edom)		-> <<"Numerical argument out of domain.">>;
error(erange)		-> <<"Result too large or too small.">>;
error(eagain)		-> <<"Resource temporarily unavailable.">>;
error(einprogress)	-> <<"Operation now in progress.">>;
error(ealready)		-> <<"Operation already in progress.">>;
error(enotsock)		-> <<"Socket operation on non-socket.">>;
error(edestaddrreq)	-> <<"Destination address required.">>;
error(emsgsize)		-> <<"Message too long.">>;
error(eprototype)	-> <<"Protocol wrong type for socket.">>;
error(enoprotoopt)	-> <<"Protocol option not available.">>;
error(eprotonosupport)	-> <<"Protocol not supported.">>;
error(esocktnosupport)	-> <<"Socket type not supported.">>;
error(eopnotsupp)	-> <<"Operation not supported.">>;
error(epfnosupport)	-> <<"Protocol family not supported.">>;
error(eafnosupport)	-> <<"Address family not supported by protocol family.">>;
error(eaddrinuse)	-> <<"Address already in use.">>;
error(eaddrnotavail)	-> <<"Cannot assign requested address.">>;
error(enetdown)		-> <<"Network is down.">>;
error(enetunreach)	-> <<"Network is unreachable.">>;
error(enetreset)	-> <<"Network dropped connection on reset.">>;
error(econnaborted)	-> <<"Software caused connection abort.">>;
error(econnreset)	-> <<"Connection reset by peer.">>;
error(enobufs)		-> <<"No buffer space available.">>;
error(eisconn)		-> <<"Socket is already connected.">>;
error(enotconn)		-> <<"Socket is not connected.">>;
error(eshutdown)	-> <<"Cannot send after socket shutdown.">>;
error(etoomanyrefs)	-> <<"Too many references: can't splice.">>;
error(etimedout)	-> <<"Operation timed out.">>;
error(econnrefused)	-> <<"Connection refused.">>;
error(eloop)		-> <<"Too many levels of symbolic links.">>;
error(enametoolong)	-> <<"File name too long.">>;
error(ehostdown)	-> <<"Host is down.">>;
error(ehostunreach)	-> <<"No route to host.">>;
error(enotempty)	-> <<"Directory not empty.">>;
error(eproclim)		-> <<"Too many processes.">>;
error(eusers)		-> <<"Too many users.">>;
error(edquot)		-> <<"Disc quota exceeded.">>;
error(estale)		-> <<"Stale NFS file handle.">>;
error(eremote)		-> <<"Too many levels of remote in path.">>;
error(ebadrpc)		-> <<"RPC struct is bad.">>;
error(erpcmismatch)	-> <<"RPC version wrong.">>;
error(eprogunavail)	-> <<"RPC prog.">>;
error(eprogmismatch)	-> <<"Program version wrong.">>;
error(eprocunavail)	-> <<"Bad procedure for program.">>;
error(enolck)		-> <<"No locks available.">>;
error(enosys)		-> <<"Function not implemented.">>;
error(eftype)		-> <<"Inappropriate file type or format.">>;
error(eauth)		-> <<"Authentication error.">>;
error(eneedauth)	-> <<"Need authenticator.">>;
error(eidrm)		-> <<"Identifier removed.">>;
error(enomsg)		-> <<"No message of the desired type.">>;
error(eoverflow)	-> <<"Value too large to be stored in data type.">>;
error(eilseq)		-> <<"Illegal byte sequence.">>;
error(enotsup)		-> <<"Not supported.">>;
error(ecanceled)	-> <<"Operation canceled.">>;
error(ebadmsg)		-> <<"Bad or corrupt message.">>;
error(enodata)		-> <<"No message available.">>;
error(enosr)		-> <<"No STREAM resources.">>;
error(enostr)		-> <<"Not a STREAM.">>;
error(etime)		-> <<"STREAM ioctl timeout.">>;
error(enoattr)		-> <<"Attribute not found.">>;
error(emultihop)	-> <<"Multihop attempted.">>;
error(enolink)		-> <<"Link has been severed.">>;
error(eproto)		-> <<"Protocol error.">>;
error(Reason)		-> atom_to_binary(Reason, utf8).

-spec tr(binary(), binary()) -> binary().
tr(Bs, FromSet) ->
	tr(Bs, FromSet, <<>>).

-spec tr(binary(), binary(), binary()) -> binary().
tr(Bs, <<>>, _ToSet) ->
	Bs;
tr(Bs, FromSet, ToSet) ->
	tr(Bs, FromSet, ToSet, <<>>).

-spec tr(binary(), binary(), binary(), binary()) -> binary().
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

-spec ftime(binary(), dtz:epochsecs() | dtz:dtz() | {dtz:date(), dtz:time()}) -> binary().
ftime(Fmt, {Date, Time}) ->
	ftime(Fmt, {Date, Time, dtz:time_zone_seconds()});
ftime(Fmt, {Date, Time, Tz}) ->
	ftime(Fmt, {Date, Time, Tz}, <<>>);
ftime(Fmt, EpochSeconds) ->
	ftime(Fmt, dtz:from_epoch_seconds(EpochSeconds)).

-spec ftime(binary(), dtz:dtz(), binary()) -> binary().
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
		{YearOfWeek0, _WeekNum} = calendar:iso_week_number(Date),
		YearOfWeek1 = pad_int(YearOfWeek0, $0, 4),
		<<Acc/binary, YearOfWeek1/binary>>;
	$g ->
		{YearOfWeek0, _WeekNum} = calendar:iso_week_number(Date),
		YearOfWeek1 = pad_int(YearOfWeek0 rem 100, $0, 2),
		<<Acc/binary, YearOfWeek1/binary>>;
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
		UTC = integer_to_binary(dtz:to_epoch_seconds({Date, Time, Tz})),
		<<Acc/binary, UTC/binary>>;
	$T ->
		IsoTime = ftime(<<"%H:%M:%S">>, {Date, Time, Tz}),
		<<Acc/binary, IsoTime/binary>>;
	$t ->
		<<Acc/binary, $\t>>;
% 	$U ->
% 		throw({enotsup, Ch});
	$u ->
		% Week day counting from Monday 1..7.
		WeekDay0 = calendar:day_of_the_week(Date),
		WeekDay1 = pad_int(WeekDay0, $0, 0),
		<<Acc/binary, WeekDay1/binary>>;
	$V ->
		% https://en.wikipedia.org/wiki/ISO_week_date
		% https://webspace.science.uu.nl/~gent0113/calendar/isocalendar.htm
		{_YearOfWeek, WeekNum0} = calendar:iso_week_number(Date),
		WeekNum1 = pad_int(WeekNum0, $0, 2),
		<<Acc/binary, WeekNum1/binary>>;
	$v ->
		Vdate = ftime(<<"%e-%b-%Y">>, {Date, Time, Tz}),
		<<Acc/binary, Vdate/binary>>;
% 	$W ->
% 		throw({enotsup, Ch});
%	$w ->
% 		throw({enotsup, Ch});
% 	$X ->
% 		throw({enotsup, Ch});
% 	$x ->
% 		throw({enotsup, Ch});
	$Y ->
		FullYr = pad_int(Year, $0, 4),
		<<Acc/binary, FullYr/binary>>;
	$y ->
		ShortYr = pad_int(Year rem 100, $0, 2),
		<<Acc/binary, ShortYr/binary>>;
% 	$Z ->
% 		throw({enotsup, Ch});
	$z ->
		TzHr = pad_sign_int(Tz div 3600, $0, 3),
		TzMn = pad_int(abs((Tz rem 3600) div 60), $0, 2),
		<<Acc/binary, TzHr/binary, TzMn/binary>>;
	$% ->
		<<Acc/binary, $%>>;
	_ ->
		throw({enotsup, Ch})
	end,
	ftime(Rest, {Date, Time, Tz}, NewAcc);
ftime(<<Ch:8, Rest/binary>>, DateTime, Acc) ->
	ftime(Rest, DateTime, <<Acc/binary, Ch:8>>).

-spec lpad(binary(), byte(), non_neg_integer()) -> binary().
lpad(Bs, _Pad, Width) when Width =< byte_size(Bs) ->
	Bs;
lpad(Bs, Pad, Width) ->
	lpad(<<Pad:8, Bs/binary>>, Pad, Width).

-spec rpad(binary(), byte(), non_neg_integer()) -> binary().
rpad(Bs, _Pad, Width) when Width =< byte_size(Bs) ->
	Bs;
rpad(Bs, Pad, Width) ->
	rpad(<<Bs/binary, Pad:8>>, Pad, Width).

-spec pad_int(integer(), char(), integer()) -> binary().
pad_int(Int, $0, Width) when Int < 0 ->
	Num = pad_int(-Int, $0, Width-1),
	<<$-, Num/binary>>;
pad_int(Int, Pad, Width) ->
	lpad(integer_to_binary(Int), Pad, Width).

-spec pad_sign_int(integer(), char(), integer()) -> binary().
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

% Similar to C's strtol().
-spec to_int(binary(), base()) -> {integer(), binary()} | {einval, base()}.
to_int(Bs, Base) ->
	to_int(Bs, Base, -1).

-spec to_int(binary(), base(), integer()) -> {integer(), binary()} | {einval, base()}.
to_int(Bs, 0, MaxDigits) ->
	to_int(Bs, 0, MaxDigits, 0);
to_int(_Bs, 1, _MaxDigits) ->
	{einval, 1};
to_int(_Bs, Base, _MaxDigits) when Base < 0 orelse 36 < Base ->
	{einval, Base};
to_int(Bs, Base, MaxDigits) ->
	to_int(Bs, Base, MaxDigits, 0).

% Handle leading prefixes and/or Base = 0 conditions.
-spec to_int(binary(), base(), integer(), integer()) -> {integer(), binary()}.
to_int(<<"0x", Rest/binary>>, 16, MaxDigits, Acc) ->
	to_int(Rest, 16, MaxDigits, Acc, 1);
to_int(<<"0x", Rest/binary>>, 0, MaxDigits, Acc) ->
	to_int(Rest, 16, MaxDigits, Acc, 1);
to_int(<<"0", Rest/binary>>, 0, MaxDigits, Acc) ->
	to_int(Rest, 8, MaxDigits, Acc, 1);
to_int(Bs, 0, MaxDigits, Acc) ->
	to_int(Bs, 10, MaxDigits, Acc);
to_int(<<$ , Rest/binary>>, Base, MaxDigits, Acc) ->
	to_int(Rest, Base, MaxDigits, Acc);
to_int(<<$-, Rest/binary>>, 10, MaxDigits, Acc) ->
	to_int(Rest, 10, MaxDigits, Acc, -1);
to_int(<<$+, Rest/binary>>, 10, MaxDigits, Acc) ->
	to_int(Rest, 10, MaxDigits, Acc, 1);
to_int(Bs, Base, MaxDigits, Acc) ->
	to_int(Bs, Base, MaxDigits, Acc, 1).

% Parse digits.
-spec to_int(binary(), base(), integer(), integer(), -1 | 1) -> {integer(), binary()}.
to_int(<<>>, _Base, _MaxDigits, Acc, Sign) ->
	{ Sign * Acc, <<>> };
to_int(Bs, _Base, 0, Acc, Sign) ->
	{ Sign * Acc, Bs };
to_int(<<Ch:8, Rest/binary>>, Base, MaxDigits, Acc, Sign) ->
	case {ctype:isbase(Ch, Base), ctype:isdigit(Ch)} of
	{true, true} ->
		to_int(Rest, Base, MaxDigits-1, Acc * Base + (Ch - $0), Sign);
	{true, false} ->
		to_int(Rest, Base, MaxDigits-1, Acc * Base + (10 + ctype:toupper(Ch) - $A), Sign);
	{false, _} ->
		{ Sign * Acc, <<Ch, Rest/binary>> }
	end.

-spec to_date_time(binary()) -> {dtz:dtz(), binary()} | badarg.
to_date_time(Bs) ->
	case iso_date_time(Bs) of
	badarg ->
		to_date_time(Bs, [
			% RFC 2822 date-time variants
			<<"%a, %d %b %Y %H:%M:%S %z">>,	% RFC 2822 date-time format.
			<<"%a, %d %b %Y %H:%M:%S">>,
			<<"%a %d %b %Y %H:%M:%S %z">>,
			<<"%a %d %b %Y %H:%M:%S">>,
			<<"%d %b %Y %H:%M:%S %z">>,
			<<"%d %b %Y %H:%M:%S">>,
			<<"%d %b %Y %H:%M %z">>,
			<<"%d %b %Y %H:%M">>,
			<<"%d %b %Y %H %M %z">>,
			<<"%d %b %Y %H %M">>,
			<<"%d %b %Y">>,

			% ctime() variants
			<<"%a, %b %d %H:%M:%S %Y %z">>,
			<<"%a, %b %d %H:%M:%S %Y">>,
			<<"%a %b %d %H:%M:%S %Y %z">>,
			<<"%a %b %d %H:%M:%S %Y">>,	% ctime() format.
			<<"%b %d %H:%M:%S %Y %z">>,
			<<"%b %d %H:%M:%S %Y">>,

			% Partial date or time.
			<<"%Y/%m/%d">>,
			<<"%b %d, %Y">>,
			<<"%b %d %Y">>,
			<<"%H:%M:%S %z">>,
			<<"%H:%M:%S">>,
			<<"%H:%M %z">>,
			<<"%H:%M">>,
			<<"%H%M">>
		]);
	DateTimeTz_Rest ->
		DateTimeTz_Rest
	end.

-spec to_date_time(binary(), [binary()]) -> {dtz:dtz(), binary()} | badarg.
to_date_time(_Bs, []) ->
	badarg;
to_date_time(Bs, [Fmt | Tail]) ->
%io:format("fmt=~s~n", [Fmt]),
	case ptime(Bs, Fmt) of
	{badarg, _Rest} ->
		to_date_time(Bs, Tail);
	{_Dtz, Bs} ->
		to_date_time(Bs, Tail);
	Result ->
		Result
	end.

%%
%% Parse the most common formats of ISO 8601.
%%
%%	YYYY[-]MM[-]DD[Thh[:]mm[:]ss[.ssssss][-zz[:]zz]
%%
-spec iso_date_time(binary()) -> {dtz:dtz(), binary()} | badarg.
iso_date_time(Bs) ->
	try
		{Date, Rest1} = iso_date(Bs),
		{Time, Tz, Rest2} = iso_time(Rest1),
		{{Date, Time, Tz}, Rest2}
	catch
		% binary_to_integer/1 fails on non-digit.
		error:badarg ->
			badarg;
		% iso_date/1 returned badarg.
		error:{badmatch, badarg} ->
			badarg
	end.

-spec iso_date(binary()) -> {calendar:date(), binary()}.
iso_date(<<Year:4/bytes, $-, Month:2/bytes, $-, Day:2/bytes, Rest/binary>>) ->
	{{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, Rest};
iso_date(<<Year:4/bytes, Month:2/bytes, Day:2/bytes, Rest/binary>>) ->
	{{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)}, Rest};
iso_date(_Other) ->
	badarg.

-spec iso_time(binary()) -> {calendar:time(), dtz:tz(), binary()}.
iso_time(<<$T, Hour:2/bytes, $:, Minute:2/bytes, $:, Second:2/bytes, Rest/binary>>) ->
	{_, Tz, Rest1} = iso_time_zone(iso_time_fraction(Rest)),
	{ {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}, Tz, Rest1 };
iso_time(<<$T, Hour:2/bytes, Minute:2/bytes, Second:2/bytes, Rest/binary>>) ->
	{_, Tz, Rest1} = iso_time_zone(iso_time_fraction(Rest)),
	{ {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)}, Tz, Rest1 };
iso_time(Other) ->
	{{0, 0, 0}, dtz:time_zone_seconds(), Other}.

-spec iso_time_fraction(binary()) -> binary().
iso_time_fraction(<<$., Rest/binary>>) ->
	{_, Rest1} = to_int(Rest, 10),
	Rest1;
iso_time_fraction(<<$,, Rest/binary>>) ->
	{_, Rest1} = to_int(Rest, 10),
	Rest1;
iso_time_fraction(Other) ->
	Other.

-spec iso_time_zone(binary()) -> {ok | badarg, dtz:tz(), binary()}.
iso_time_zone(<<>>) ->
	% Nothing to consume, assume local time zone.
	{ok, dtz:time_zone_seconds(), <<>>};
iso_time_zone(<<$Z, Rest/binary>>) ->
	{ok, 0, Rest};
iso_time_zone(<<$-, TzHr:2/bytes, $:, TzMn:2/bytes, Rest/binary>>) ->
	{ok, -1 * (binary_to_integer(TzHr) * 3600 + binary_to_integer(TzMn) * 60), Rest};
iso_time_zone(<<$+, TzHr:2/bytes, $:, TzMn:2/bytes, Rest/binary>>) ->
	{ok, (binary_to_integer(TzHr) * 3600 + binary_to_integer(TzMn) * 60), Rest};
iso_time_zone(<<$-, TzHr:2/bytes, TzMn:2/bytes, Rest/binary>>) ->
	{ok, -1 * (binary_to_integer(TzHr) * 3600 + binary_to_integer(TzMn) * 60), Rest};
iso_time_zone(<<$+, TzHr:2/bytes, TzMn:2/bytes, Rest/binary>>) ->
	{ok, (binary_to_integer(TzHr) * 3600 + binary_to_integer(TzMn) * 60), Rest};
iso_time_zone(Other) ->
	% No time zone parsed, assume local time zone.
	{badarg, dtz:time_zone_seconds(), Other}.

-spec ptime(binary(), binary()) -> {dtz:dtz(), binary()} | {badarg, binary()}.
ptime(Bs, Fmt) ->
%	ptime(Bs, Fmt, {{0, 0, 0}, {0, 0, 0}, 0}).
	ptime(Bs, Fmt, {{0, 0, 0}, {0, 0, 0}, dtz:time_zone_seconds()}).

-spec ptime(binary(), binary(), dtz:dtz0()) -> {dtz:dtz(), binary()} | {badarg, binary()}.
ptime(Bs, <<>>, Dtz) ->
	{Dtz, Bs};
ptime(Bs, <<$ , Fmt/binary>>, Dtz) ->
	ptime(ltrim(Bs), Fmt, Dtz);
ptime(Bs, <<$\t, Fmt/binary>>, Dtz) ->
	ptime(ltrim(Bs), Fmt, Dtz);
ptime(Bs, <<"%", Ch:8, Fmt/binary>>, {Date = {Year, Month, Day}, Time = {Hour, Minute, Second}, Tz}) ->
	{ DateTimeTz, Rest1 } = case Ch of
	$a ->
		Span = cspn(Bs, <<", \t\n\r\v\f">>),
		Token = sub(Bs, 0, Span),
		case index_of_word(Token, [?WEEK_DAYS_SHORT, ?WEEK_DAYS_FULL]) of
		notfound ->
			{badarg, Bs};
		_Index ->
			{{Date, Time, Tz}, sub(Bs, Span)}
		end;
	$A ->
		ptime(Bs, <<"%a">>, {Date, Time, Tz});
	$b ->
		Span = cspn(Bs, ?WHITESPACE),
		Token = sub(Bs, 0, Span),
		case index_of_word(Token, [?MONTH_SHORT, ?MONTH_FULL]) of
		notfound ->
			{badarg, Bs};
		Index ->
			{{{Year, Index rem 12 + 1, Day}, Time, Tz}, sub(Bs, Span)}
		end;
	$B ->
		ptime(Bs, <<"%b">>, {Date, Time, Tz});
	$c ->
		ptime(Bs, <<"%e %b %Y %H:%M:%S">>, {Date, Time, Tz});
% 	$C ->
% Archaic and in combo with %y (%C%y) relies on %y knowing preceeding format context.
% 		case to_int(Bs, 10, 2) of
% 		{0, Bs} ->
% 			{badarg, Bs};
% 		{Century, Rest} when 0 =< Century andalso Century =< 99 ->
% 			{{{Century * 100, Month, Day}, Time, Tz}, Rest};
% 		_ ->
% 			{badarg, Bs}
% 		end;
	$d ->
		case to_int(Bs, 10, 2) of
		{NewDay, Rest} when 1 =< NewDay andalso NewDay =< 31 ->
			{{{Year, Month, NewDay}, Time, Tz}, Rest};
		_Other ->
			{badarg, Bs}
		end;
	$D ->
		ptime(Bs, <<"%m/%d/%y">>, {Date, Time, Tz});
	$e ->
		ptime(Bs, <<"%d">>, {Date, Time, Tz});
	$F ->
		ptime(Bs, <<"%Y-%m-%d">>, {Date, Time, Tz});
% 	$G ->
% 		throw({enotsup, Ch});
% 	$g ->
% 		throw({enotsup, Ch});
	$h ->
		ptime(Bs, <<"%b">>, {Date, Time, Tz});
	$H ->
		case to_int(Bs, 10, 2) of
		{NewHour, Rest} when 0 =< NewHour andalso NewHour =< 23 ->
			{{Date, {NewHour, Minute, Second}, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$I ->
		case to_int(Bs, 10, 2) of
		{NewHour, Rest} when 1 =< NewHour andalso NewHour =< 12 ->
			{{Date, {NewHour, Minute, Second}, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$k ->
		ptime(Bs, <<"%H">>, {Date, Time, Tz});
	$l ->
		ptime(Bs, <<"%I">>, {Date, Time, Tz});
	$j ->
		case to_int(Bs, 10, 3) of
		{DayOfYear, Rest} when 1 =< DayOfYear andalso DayOfYear =< 366 ->
			% Ignored for now.
			{{Date, Time, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$m ->
		case to_int(Bs, 10, 2) of
		{NewMonth, Rest} when 1 =< NewMonth andalso NewMonth =< 12 ->
			{{{Year, NewMonth, Day}, Time, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$M ->
		case to_int(Bs, 10, 2) of
		{NewMinute, Rest} when 0 =< NewMinute andalso NewMinute =< 59 ->
			{{Date, {Hour, NewMinute, Second}, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$n ->
		Span = spn(Bs, ?WHITESPACE),
		{{Date, Time, Tz}, sub(Bs, Span)};
	$p ->
		Span = cspn(Bs, ?LINESPACE),
		Rest = sub(Bs, Span),
		case index_of_word(sub(Bs, 0, Span), [<<"am">>, <<"pm">>]) of
		0 ->
			{{Date, Time, Tz}, Rest};
		1 when 0 < Hour andalso Hour =< 12->
			{{Date, {(Hour + 12) rem 24, Minute, Second}, Tz}, Rest};
		1 ->
			{{Date, Time, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$r ->
		ptime(Bs, <<"%l:%M %p">>, {Date, Time, Tz});
	$R ->
		ptime(Bs, <<"%H:%M">>, {Date, Time, Tz});
	$S ->
		case to_int(Bs, 10, 2) of
		{NewSecond, Rest} when 0 =< NewSecond andalso NewSecond =< 60 ->
			{{Date, {Hour, Minute, NewSecond}, Tz}, Rest};
		_ ->
			{badarg, Bs}
		end;
	$s ->
		case to_int(Bs, 10) of
		{0, Bs} ->
			% Nothing consumed.
			{badarg, Bs};
		{Esecs, Rest} ->
			{dtz:to_local(dtz:from_epoch_seconds(Esecs)), Rest}
		end;
	$t ->
		Span = spn(Bs, ?WHITESPACE),
		{{Date, Time, Tz}, sub(Bs, Span)};
	$T ->
		ptime(Bs, <<"%H:%M:%S">>, {Date, Time, Tz});
% 	$U ->
% 		throw({enotsup, Ch});
% 	$u ->
% 		throw({enotsup, Ch});
% 	$V ->
% 		throw({enotsup, Ch});
% 	$v ->
% 		throw({enotsup, Ch});
% 	$W ->
% 		throw({enotsup, Ch});
% 	$w ->
% 		throw({enotsup, Ch});
% 	$X ->
% 		throw({enotsup, Ch});
% 	$x ->
% 		throw({enotsup, Ch});
	$Y ->
		{NewYear, Rest} = to_int(Bs, 10),
		{{{NewYear, Month, Day}, Time, Tz}, Rest};
	$y ->
		% http://pubs.opengroup.org/onlinepubs/9699919799/
		case to_int(Bs, 10, 2) of
		{Value, Rest} when Value =< 68 ->
			{{{2000 + Value, Month, Day}, Time, Tz}, Rest};
		{Value, Rest} ->
			{{{1900 + Value, Month, Day}, Time, Tz}, Rest}
		end;
% 	$Z ->
% 		throw({enotsup, Ch});
	$z ->
		case iso_time_zone(Bs) of
		{ok, NewTz, Rest} ->
			{{Date, Time, NewTz}, Rest};
		{badarg, _NewTz, _} ->
			{badarg, Bs}
		end;
	$% ->
		<<Pct:8, Rest/binary>> = Bs,
		case Pct == $% of
		true ->
			{{Date, Time, Tz}, Rest};
		false ->
			{badarg, Bs}
		end;
	_ ->
		throw({enotsup, Ch})
	end,
	ptime(Rest1, Fmt, DateTimeTz);
ptime(<<Ch:8, Rest/binary>>, <<Ch:8, Fmt/binary>>, DateTimeTz) ->
	ptime(Rest, Fmt, DateTimeTz);
ptime(Bs, _Fmt, _DateTimeTz) ->
	{badarg, Bs}.

-spec index_of_word(binary(), [binary()]) -> notfound | non_neg_integer().
index_of_word(Word, List) ->
	index_of_word(Word, List, 0).

-spec index_of_word(binary(), [binary()], non_neg_integer()) -> notfound | non_neg_integer().
index_of_word(_, [], _) ->
	notfound;
index_of_word(Word, [Head | Tail], Index) ->
	case casecmp(Word, Head) of
	0 ->
		Index;
	_ ->
		index_of_word(Word, Tail, Index + 1)
	end.

-spec str(binary(), binary()) -> integer().
str(Bs, Pattern) ->
	str(Bs, Pattern, Bs, Pattern, 0).

-spec str(binary(), binary(), binary(), binary(), integer()) -> integer().
str(_Bs, _Pattern, _, <<>>, Index) ->
	% Reached end of pattern.
	Index;
str(<<>>, _Pattern, _, _, _) ->
	% Reached end of string before end of pattern.
	-1;
str(Bs, Pattern, <<Ch:8, Next/binary>>, <<Ch:8, Pat/binary>>, Index) ->
	% Matched characters, advance pattern.
	str(Bs, Pattern, Next, Pat, Index);
str(<<_:8, Rest/binary>>, Pattern, _, _, Index) ->
	% Mismatched characters, reset pattern.
	str(Rest, Pattern, Rest, Pattern, Index + 1).

-spec casestr(binary(), binary()) -> integer().
casestr(Bs, Pattern) ->
	casestr(Bs, Pattern, Bs, Pattern, 0).

-spec casestr(binary(), binary(), binary(), binary(), integer()) -> integer().
casestr(_Bs, _Pattern, _, <<>>, Index) ->
	% Reached end of pattern.
	Index;
casestr(<<>>, _Pattern, _, _, _) ->
	% Reached end of string before end of pattern.
	-1;
casestr(Bs, Pattern, <<Ach:8, Next/binary>>, <<Bch:8, Pat/binary>>, Index) ->
	UpperA = ctype:toupper(Ach),
	UpperB = ctype:toupper(Bch),
	if
		UpperA == UpperB ->
			casestr(Bs, Pattern, Next, Pat, Index);
		UpperA /= UpperB ->
			<<_:8, Rest/binary>> = Bs,
			casestr(Rest, Pattern, Rest, Pattern, Index + 1)
	end.

-spec isprintable(binary()) -> boolean().
isprintable(<<>>) ->
	true;
isprintable(<<Ch:8, Rest/binary>>) ->
	case ctype:isprint(Ch) orelse ctype:isspace(Ch) orelse Ch == ?BEL orelse Ch == ?BS orelse Ch == ?ESC of
	true ->
		isprintable(Rest);
	false ->
		false
	end;
isprintable(_Other) ->
	false.

-define(DQUOTE, 16#22).
-define(SQUOTE, 16#27).
-define(BACKSLASH, 16#5C).

-spec token(binary()) -> {binary(), binary()}.
token(Bs) ->
	token(Bs, ?WHITESPACE).

-spec token(binary(), binary()) -> {binary(), binary()}.
token(Bs, Delims) ->
	token(Bs, Delims, <<>>).

-spec token(binary(), binary(), binary()) -> {binary(), binary()}.
token(<<>>, _Delims, Acc) ->
	{Acc, <<>>};
token(Bs, Delims, Acc) ->
	case Bs of
	% Trailing backslash at end of string is ignored.
	<<?BACKSLASH>> ->
		token(<<>>, Delims, Acc);
	% Backslash escape next character.
	<<?BACKSLASH, Octet:8, Rest/binary>> ->
		token(Rest, Delims, <<Acc/binary, Octet:8>>);
	% Start double-quote segement.
	<<?DQUOTE, Rest/binary>> ->
		token(Rest, Delims, ?DQUOTE, Acc);
	% Start single-quote segement.
	<<?SQUOTE, Rest/binary>> ->
		token(Rest, Delims, ?SQUOTE, Acc);
	<<Octet:8, Rest/binary>> ->
		% Is character from set of delimiters?
		case str:chr(Delims, Octet) of
		-1 ->
			% Collect character.
			token(Rest, Delims, <<Acc/binary, Octet:8>>);
		_ ->
			% Eat trailing whitespace following delimiter. RFC 4180
			% says leading and trailing whitespace are significant,
			% but that can be solved by quoting the whitespace.
			Span = spn(Rest, ?WHITESPACE),
			<<_:Span/bytes, Rest2/binary>> = Rest,
			{Acc, Rest2}
		end
	end.

-spec token(<<_:8,_:_*8>>, binary(), ?DQUOTE|?SQUOTE, binary()) -> {binary(), binary()}.
token(<<>>, _Delims, Quote, Acc) ->
	throw({error, unbalanced_quotes, Quote, Acc});
token(Bs, Delims, Quote, Acc) ->
	case Bs of
	% Within quoted string, use paired quotes for a literal quote.
	<<Quote:8, Quote:8, Rest/binary>> ->
		token(Rest, Delims, Quote, <<Acc/binary, Quote:8>>);
	% End of quoted segment.
	<<Quote:8, Rest/binary>> ->
		token(Rest, Delims, Acc);
	% Collect character.
	<<Octet:8, Rest/binary>> ->
		token(Rest, Delims, Quote, <<Acc/binary, Octet:8>>)
	end.

-spec split(binary()) -> [binary()].
split(Bs) ->
	split(Bs, ?WHITESPACE).

-spec split(binary(), binary()) -> [binary()].
split(Bs, Delims) ->
	split(Bs, Delims, []).

-spec split(binary(), binary(), [binary()]) -> [binary()].
split(<<>>, _Delims, Acc) ->
	lists:reverse(Acc);
split(Bs, Delims, Acc) ->
	{Value, Rest} = token(Bs, Delims),
	split(Rest, Delims, [Value | Acc]).

