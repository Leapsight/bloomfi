-module(prop_bitvector).
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").



%% Property: new/1 creates a bit array of the correct size
prop_new_test() ->
    ?FORALL(Size, pos_integer(),
        begin
            A = bitvector:new(Size),
            bitvector:size(A) == Size
        end).

%% Property: set/2 and get/2 are consistent
prop_set_get_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        begin
            A = bitvector:new(Size),
            A1 = bitvector:set(Index, A),
            true == bitvector:get(Index, A1)
        end).

%% Property: size/1 always matches the expected dimension
prop_size_test() ->
    ?FORALL(Size, pos_integer(),
        begin
            A = bitvector:new(Size),
            bitvector:size(A) == Size
        end).

%% Property: clear/2 sets a bit to 0 and doesn't affect other bits
prop_clear_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        Size > Index andalso
        begin
            A = bitvector:new(Size),
            A1 = bitvector:set(Index, A), %% Set the bit first
            A2 = bitvector:clear(Index, A1), %% Clear the bit
            bitvector:get(Index, A2) =:= false %% Ensure it is cleared
        end).

%% Property: flip/2 toggles a bit and can restore the original state with a
%% second flip
prop_flip_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        Size > Index andalso
        begin
            A = bitvector:new(Size),
            A1 = bitvector:flip(Index, A),
            A2 = bitvector:flip(Index, A1),
            %% Verify state is restored
            bitvector:get(Index, A) =:= bitvector:get(Index, A2)
        end).




%% =============================================================================
%% GENERATORS
%% =============================================================================

%% Generator for bounded Index and Size
bounded_index() ->
    ?LET(Size, pos_integer(),
        ?LET(Index, choose(0, Size - 1),
            {Size, Index})).

