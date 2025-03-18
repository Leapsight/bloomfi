-module(bloomfi).
-moduledoc("""
Based on Scalable Bloom Filters, Paulo Sérgio Almeida, Carlos Baquero, Nuno
Preguiça, David Hutchison. Information Processing Letters Volume 101, Issue 6,
31 March 2007, Pages 255-261

Provides scalable bloom filters that can grow indefinitely while
ensuring a desired maximum false positive probability. Also provides
standard partitioned bloom filters with a maximum capacity.

Bitvectors are dimensioned as a power of 2 to enable reusing hash values
across filters through bit operations. Double hashing is used (no need for
enhanced double hashing for partitioned bloom filters).
""").

-record(?MODULE, {
    size        ::  pos_integer(),
    %% error probability
    p           ::  float(),
    %% maximum number of elements
    capacity    ::  pos_integer(),
    %% 2^slice_size = m, the size of each slice (bitvector)
    slice_size  ::  pos_integer(),
    store       ::  module(),
    %% This bloom filter implementation consists of partitioning the M bits
    %% among the k hash functions, thus creating k slices of m =
    %% M/k bits.
    %% Each hash function hi, with 1 < i < k, produces an index
    %% over m for its respective slice.
    slices      ::  [bitvector:t()]
}).

-type t()       ::  #?MODULE{}.

-export_type([t/0]).

-export([add/2]).
-export([capacity/1]).
-export([member/2]).
-export([memory/1]).
-export([new/1]).
-export([new/2]).
-export([new/3]).
-export([size/1]).
-export([union/2]).


%% =============================================================================
%% API
%% =============================================================================


-doc("""
Returns a new bloom filter with fixed capacity based on the
requested `Capacity' and error probability of `0.001'.
The actual capacity will be equal or greater than the requested one.
""").
-spec new(Capacity :: pos_integer()) -> t().

new(Capacity) ->
    new(Capacity, 0.001).


-doc("""
Returns a new bloom filter with fixed capacity `Capacity' and error
probability of `P'.
""").

new(Capacity0, P) when
is_number(Capacity0), Capacity0 > 0, is_float(P), P > 0, P < 1 ->
    %% N >= 4/rule of thumb; due to double hashing
    Capacity = case Capacity0 >= 4/P of
        true ->
            Capacity0;
        false ->
            4/P
    end,
    new(size, Capacity, P).


-doc("""
Returns a new bloom filter with fixed capacity `Capacity' and error
probability of `P'.
""").
-spec new(size | bits, Capacity :: pos_integer(), P :: float()) ->
    t() | no_return().

new(bits, Capacity, P) ->
    SliceCount = 1 + trunc(log2(1 / P)),
    init(SliceCount, Capacity, P);

new(size, Capacity, P) ->
    SliceCount = 1 + trunc(log2(1 / P)),
    FPP = math:pow(P, 1 / SliceCount),
    SliceSize = 1 + trunc(-log2(1 - math:pow(1 - FPP, 1 / Capacity))),
    init(SliceCount, SliceSize, P).


-spec size(t()) -> pos_integer().

size(#?MODULE{size = Size}) -> Size.


-spec capacity(t()) -> pos_integer().

capacity(#?MODULE{capacity = Capacity}) -> Capacity.


member(Element, #?MODULE{slice_size = SliceSize} = BF) ->
    hash_member(make_hashes(SliceSize, Element), BF).


add(Element, #?MODULE{slice_size = SliceSize} = BF) ->
    hash_add(make_hashes(SliceSize, Element), BF).


-spec union(t(), t()) -> t() | no_return().

union(
    #?MODULE{capacity = N, slice_size = S, slices = Sa} = _A,
    #?MODULE{capacity = N, slice_size = S, slices = Sb} = _B
) when length(Sa) ==  length(Sb) ->
    error(not_implemented);

union(_, _) ->
    error(badarg).


-spec memory(t()) -> Bytes :: non_neg_integer().

memory(#?MODULE{slices = Slices}) ->
    lists:sum([bitvector:memory(Slice) || Slice <- Slices]).


%% =============================================================================
%% PRIVATE
%% =============================================================================



init(SliceCount, SliceSize, P) when is_integer(SliceSize) ->
    FPP = math:pow(P, 1 / SliceCount),
    M = 1 bsl SliceSize,
    Capacity = trunc(math:log(1 - FPP) / math:log(1 - 1 / M)),
    #?MODULE{
        store = bitvector,
        size = 0,
        p = P,
        capacity = Capacity,
        slice_size = SliceSize,
        slices = [
            bitvector:new(1 bsl SliceSize) || _ <- lists:seq(1, SliceCount)
        ]
    }.


%% @private
log2(X) ->
    math:log(X) / math:log(2).


%% @private
hash_add(Hashes, BF) ->
    Mask = 1 bsl BF#?MODULE.slice_size - 1,
    {I1, I0} = make_indexes(Mask, Hashes),

    case all_set(Mask, I1, I0, BF#?MODULE.slices) of
        true ->
            BF;
        false ->
            BF#?MODULE{
                size = BF#?MODULE.size + 1,
                slices = set_bits(Mask, I1, I0, BF#?MODULE.slices, [])
            }
    end.


%% @private
set_bits(_Mask, _I1, _I, [], Acc) ->
    lists:reverse(Acc);

set_bits(Mask, I1, I, [H|T], Acc) ->
    _ = bitvector:set(I, H),
    set_bits(Mask, I1, (I + I1) band Mask, T, [H|Acc]).



%% @private
make_hashes(SliceSize, Element) when SliceSize =< 16 ->
    erlang:phash2({Element}, 1 bsl 32);

make_hashes(SliceSize, Element) when SliceSize =< 32 ->
    {erlang:phash2({Element}, 1 bsl 32), erlang:phash2([Element], 1 bsl 32)}.


%% @private
hash_member(Hashes, #?MODULE{slice_size = SliceSize, slices = Slices}) ->
    Mask = 1 bsl SliceSize - 1,
    {I1, I0} = make_indexes(Mask, Hashes),
    all_set(Mask, I1, I0, Slices).


%% @private
make_indexes(Mask, {H0, H1}) when Mask > 1 bsl 16 ->
    masked_pair(Mask, H0, H1);

make_indexes(Mask, {H0, _}) ->
    make_indexes(Mask, H0);

make_indexes(Mask, H0) ->
    masked_pair(Mask, H0 bsr 16, H0).


%% @private
masked_pair(Mask, X, Y) ->
    {X band Mask, Y band Mask}.


%% @private
all_set(_Mask, _I1, _I, []) ->
    true;

all_set(Mask, I1, I, [H|T]) ->
    case bitvector:get(I, H) of
        true -> all_set(Mask, I1, (I + I1) band Mask, T);
        false -> false
    end.