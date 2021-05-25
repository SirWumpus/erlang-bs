%%
%% Boyer-Moyer-Sunday Algorithm
%%
%% This program implements a generalised Boyer-Moore-Sunday approximate
%% string matching for k-mismatches.  For k=0, the program performs exact
%% string searching.  The implemntation turns out to be slightly easier
%% than the Horspool version presented by Tarhio & Ukkonen or Kuei-Hao
%% Chen's slides.
%%
-module(sunday).
-export([init/2, search/2, search/3]).

-type pattern()		:: binary().
-type index()		:: non_neg_integer().
-type maxerr()		:: non_neg_integer().
-type err_k()		:: non_neg_integer().
-type shift()		:: pos_integer().
-type shifttable()	:: #{byte() | other => shift()}.
-type deltamap()	:: #{err_k() => shifttable()}.
-type tables()		:: {pattern(), maxerr(), deltamap()}.

-spec init(pattern(), maxerr()) -> tables() | badarg.
init(Pattern, MaxErr) when MaxErr < 0 orelse byte_size(Pattern) =< MaxErr ->
	badarg;
init(Pattern, MaxErr) ->
	{ Pattern, MaxErr, init(Pattern, MaxErr, 0, #{}) }.

-spec init(pattern(), maxerr(), err_k(), deltamap()) -> deltamap().
init(_, MaxErr, K, DeltaMap) when MaxErr < K ->
	DeltaMap;
init(Pattern, MaxErr, K, DeltaMap) ->
	init(Pattern, MaxErr, K+1, DeltaMap#{K => init_row(Pattern, K)}).

-spec init_row(pattern(), err_k()) -> shifttable().
init_row(Pattern, K) ->
	init_row(Pattern, K, 0, #{other => byte_size(Pattern) + 1 - K}).

-spec init_row(pattern(), err_k(), index(), shifttable()) -> shifttable().
init_row(Pattern, K, Index, RowMap) when byte_size(Pattern) - K =< Index ->
	RowMap;
init_row(Pattern, K, Index, RowMap) ->
	init_row(Pattern, K, Index+1, RowMap#{str:at(Pattern, Index) => byte_size(Pattern) - Index - K}).

-spec delta(err_k(), byte(), deltamap()) -> shift().
delta(Err, Ch, Deltas) ->
	#{Err := Row} = Deltas,
	case Row of
	#{Ch := Delta} ->
		Delta;
	#{other := Delta} ->
		Delta
	end.

-spec search(binary(), pattern() | tables() | badarg) -> index() | -1 | badarg.
search(_Bs, badarg) ->
	badarg;
search(Bs, {Pattern, MaxErr, DeltaMap}) ->
%io:format("BsLen ~B PatLen ~B DeltaMap ~w~n", [byte_size(Bs), byte_size(Pattern), DeltaMap]),
	search(Bs, Pattern, MaxErr, 0, DeltaMap);
search(Bs, Pattern) ->
	search(Bs, init(Pattern, 0)).

-spec search(binary(), pattern(), maxerr()) -> index() | -1 | badarg.
search(Bs, Pattern, MaxErr) ->
	search(Bs, init(Pattern, MaxErr)).

-spec search(binary(), pattern(), maxerr(), index(), deltamap()) -> index() | -1.
search(Bs, Pattern, _MaxErr, Offset, _DeltaMap) when Offset > byte_size(Bs) - byte_size(Pattern) ->
	% Opted for -1 to remain compatible with str:str et al.
	-1;
search(Bs, Pattern, MaxErr, Offset, DeltaMap) ->
	StartDelta = byte_size(Pattern) + 1 - MaxErr,
%io:format("[~s][~s] max ~B off ~B delta ~B~n", [Bs, Pattern, MaxErr, Offset, StartDelta]),
	case cmp(str:sub(Bs, Offset), Pattern, MaxErr, 0, StartDelta, DeltaMap) of
	match ->
		Offset;
	NextDelta ->
%io:format("Shift ~B~n", [NextDelta]),
		search(Bs, Pattern, MaxErr, Offset + NextDelta, DeltaMap)
	end.

-spec cmp(binary(), pattern(), maxerr(), err_k(), shift(), deltamap()) -> match | shift().
cmp(_Bs, _Pattern, MaxErr, Err, Delta, _DeltaMap) when MaxErr < Err ->
	% Too many mismatches, return delta shift.
	Delta;
cmp(_Bs, <<>>, _MaxErr, _Err, _Delta, _DeltaMap) ->
	% End of pattern reached successfully.
	match;
cmp(<<Bch:8, Brest/binary>>, <<Pch:8, Prest/binary>>, MaxErr, Err, Delta, DeltaMap) ->
	{NewErr, NewDelta} = if
	Bch /= Pch ->
		Nch = str:at(Brest, byte_size(Prest) - Err),
		{Err+1, min(Delta, delta(Err, Nch, DeltaMap))};
	Bch == Pch ->
		{Err, Delta}
	end,
	cmp(Brest, Prest, MaxErr, NewErr, NewDelta, DeltaMap).
