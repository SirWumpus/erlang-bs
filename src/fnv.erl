%%%
%%% Fowler-Noll-Vo Hash Functions
%%%

-module(fnv).
-export([hash32/1, hash56/1]).

-define(FNV32_SHIFT, true).
-define(FNV56_SHIFT, true).

%
% FNV1a
%
% https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
% http://programmers.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed
% http://www.isthe.com/chongo/tech/comp/fnv/index.html
%
%	Hash Size	Prime				Offset
%	32-bit		16777619	0x01000193	2166136261		0x811C9DC5
%	56-bit		4294967597	0x10000012D	65300513888988631	0xE7FE78AE00EDD7
%	64-bit		1099511628211	0x100000001b3	14695981039346656037	0xCBF29CE34244AD25
%

-define(FNV32_PRIME, 16777619).
-define(FNV32_INIT, 2166136261).
-define(FNV32_MASK, 16#FFFFFFFF).

-spec hash32(binary()) -> integer().
hash32(Bs) ->
	hash32(Bs, ?FNV32_INIT).

-spec hash32(binary(), integer()) -> integer().
-ifdef(FNV32_SHIFT).

hash32(<<>>, Acc) ->
	Acc;
hash32(<<Octet:8, Rest/binary>>, Acc) ->
	H = Acc bxor Octet,
	H1 = H + ((H bsl 1) + (H bsl 4) + (H bsl 7) + (H bsl 8) + (H bsl 24)),
	hash32(Rest, H1 band ?FNV32_MASK).

-else.

hash32(<<>>, Acc) ->
	Acc;
hash32(<<Octet:8, Rest/binary>>, Acc) ->
	hash32(Rest, ((Acc bxor Octet) * ?FNV32_PRIME) band ?FNV32_MASK).

-endif.

% On 2017-04-16 14:51, Landon Curt Noll wrote:
% > Hello Anthony,
% >
% > An FNV prime for 56 bits might be possible using:
% >
% > 	https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function#FNV_prime
% > 	http://www.isthe.com/chongo/tech/comp/fnv/index.html#fnv-prime
% >
% > The tricky bit is that t.  The "easy to state” form is that t = floor((5 + 2^s) / 12),
% > which does not work.
% >
% > For 56-bits, you will need a slightly different form:
% >
% > 	t = 256^4 + 2^8 + b
% >
% > The 1st prime in the open interval (256^4 + 2^8, 256^4 + 2^9) that
% > satisfies the FNV 2nd criterion is:
% >
% > 	p = 4294967597 = 0b100000000000000000000000100101101
% >
% > and p mod (2^40 - 2^24 - 1) = 4294967597 and is > (2^24 + 2^8 + 2^7) = 16777600.
% >
% > So for 56-bits, use:
% >
% > 	FNV_Prime = 4294967597
% >
% > Now you the FNV_offset_basis, which is the FNV_0 hash of:
% >
% > 	chongo <Landon Curt Noll> /\../\
% >
% > Now the FNV_0 hash:
% >
% > 	https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function#FNV-0_hash_.28deprecated.29
% >
% > Using calc:
% >
% > 	http://www.isthe.com/chongo/tech/comp/calc/index.html
% >
% > hash = 0;
% > s = "chongo <Landon Curt Noll> /\\../\\";
% > for (i=0; i < strlen(s); ++i) {
% >     hash = xor(hash, ord(s[i]));
% >     hash = (hash * 4294967597) % (2^56);
% > }
% >
% > This produces:
% >
% > 	FNV_Offset_basis= 65300513888988631
% >
% > Of course, when you compute a 56-bit FNV using the above, you will need to compute
% > values mod 2^56 when you perform the FNV-1a operation (which is what I’d recommend).
% >
% > =-=
% >
% > A 60-bit prime will be a bit more tricky.  Unfortunately we need t to be of the form:
% >
% > 	t = 16^9 + 2^8 + b
% >
% > which is not quite what you want for hashing octets.  And worse still, in the interval (16^9 + 2^8, 16^9 + 2^9)
% > there are no primes that satisfies the FNV 2nd criterion.
% >
% > Is this for a machine with “octets” that are 4 bits instead of 8 bits?
% >
% > If you really need a 60-bit hash, I can do some more work to find one. Let me know.
% >
% > If that 56-bit hash (which requires you to compute FNV-1a using mod 2^56) is OK, then use that one.
% >
% > I hope this helps.
% >
% > chongo (Landon Curt Noll) /\oo/\
% >
% > =-=
% >
% >> On 2017-Apr-13, at 15:20, Anthony Howe <achowe@snert.com> wrote:
% >>
% >> I've been playing around with Erlang for the past couple of months now
% >> and have been porting some C programs as exercises  I was going to have
% >> a go with npdif which uses FNV hash.
% >>
% >> However, on 64bit machine, Erlang only uses 60 bits of a native word
% >> before it kicks it up to bignum implementation.
% >>
% >> http://erlang.org/doc/efficiency_guide/advanced.html#id70488
% >>
% >> In your notes on FNV for changing the hash size to compute the next
% >> largest hash  then xor fold the top bits against the low end.
% >>
% >> http://www.isthe.com/chongo/tech/comp/fnv/index.html#xor-fold
% >>
% >> However in Erlang computing a 64bit hash means switching to bignums
% >> internally.
% >>
% >> Q: Is there some other means to create a 60-bit (or 56bit) hash without
% >> relying on the next largest power of 2 and downgrading it?
% >>
% >> (Or should I just use a 32bit hash?)
% >>

-define(FNV56_PRIME, 4294967597).
-define(FNV56_INIT, 65300513888988631).
-define(FNV56_MASK, 16#00FFFFFFFFFFFFFF).

-spec hash56(binary()) -> integer().
hash56(Bs) ->
	hash56(Bs, ?FNV56_INIT).

-spec hash56(binary(), integer()) -> integer().
-ifdef(FNV56_SHIFT).

hash56(<<>>, Acc) ->
	Acc;
hash56(<<Octet:8, Rest/binary>>, Acc) ->
	H = Acc bxor Octet,
	H1 = H + ((H bsl 2) + (H bsl 3) + (H bsl 5) + (H bsl 8) + (H bsl 32)),
	hash56(Rest, H1 band ?FNV56_MASK).

-else.

hash56(<<>>, Acc) ->
	Acc;
hash56(<<Octet:8, Rest/binary>>, Acc) ->
	hash56(Rest, ((Acc bxor Octet) * ?FNV56_PRIME) band ?FNV56_MASK).

-endif.
