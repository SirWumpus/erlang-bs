-module(str).
-compile({no_auto_import,[error/1]}).
-export([
	at/2, cat/2, ncat/3, cmp/2, ncmp/3, cpy/1, ncpy/2, chr/2, rchr/2, error/1,
	len/1, rev/1, ltrim/1, rtrim/1, trim/1, spn/2, cspn/2, sub/2, sub/3, tok/2,
	casecmp/2, ncasecmp/3
]).

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
	TokLen = cspn(Bs, Delims),
	<<Token:TokLen/binary, Rest/binary>> = Bs,
	SepLen = spn(Rest, Delims),
	<<_:SepLen/binary, Rest2/binary>> = Rest,
	{Token, Rest2}.

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
