-module(prop_bitarray).
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").



%% Property: new/1 creates a bit array of the correct size
prop_new_test() ->
    ?FORALL(Size, pos_integer(),
        begin
            A = bitarray:new(Size),
            bitarray:size(A) == Size
        end).

%% Property: set/2 and get/2 are consistent
prop_set_get_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        begin
            A = bitarray:new(Size),
            A1 = bitarray:set(Index, A),
            true == bitarray:get(Index, A1)
        end).

%% Property: size/1 always matches the expected dimension
prop_size_test() ->
    ?FORALL(Size, pos_integer(),
        begin
            A = bitarray:new(Size),
            bitarray:size(A) == Size
        end).

%% Property: clear/2 sets a bit to 0 and doesn't affect other bits
prop_clear_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        Size > Index andalso
        begin
            A = bitarray:new(Size),
            A1 = bitarray:set(Index, A), %% Set the bit first
            A2 = bitarray:clear(Index, A1), %% Clear the bit
            bitarray:get(Index, A2) =:= false %% Ensure it is cleared
        end).

%% Property: flip/2 toggles a bit and can restore the original state with a
%% second flip
prop_flip_test() ->
    ?FORALL({Size, Index}, bounded_index(),
        Size > Index andalso
        begin
            A = bitarray:new(Size),
            A1 = bitarray:flip(Index, A),
            A2 = bitarray:flip(Index, A1),
            %% Verify state is restored
            bitarray:get(Index, A) =:= bitarray:get(Index, A2)
        end).




%% =============================================================================
%% GENERATORS
%% =============================================================================

%% Generator for bounded Index and Size
bounded_index() ->
    ?LET(Size, pos_integer(),
        ?LET(Index, choose(0, Size - 1),
            {Size, Index})).

