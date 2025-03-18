%% =============================================================================
%%  bitarray.erl
% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% Bit arrays
%% are dimensioned as a power of 2 to enable reusing hash values across
%% filters through bit operations. Double hashing is used (no need for
%% enhanced double hashing for partitioned bloom filters).
%% @end
%% -----------------------------------------------------------------------------
-module(bitarray).

-define(W, 27).

-type t() :: array:array().

-export([clear/2]).
-export([flip/2]).
-export([get/2]).
%% -export([intersection/2]).
-export([new/1]).
%% -export([print/1]).
-export([set/2]).
-export([size/1]).
%% -export([union/2]).




%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
new(Size) ->
  A = array:new((Size - 1) div ?W + 1, {default, 0}),
  {?MODULE, Size, A}.

%% -----------------------------------------------------------------------------
%% @doc Returns the size of the bit array.
%% @end
%% -----------------------------------------------------------------------------
size({?MODULE, Size, _}) ->
  Size.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
set(I, {?MODULE, Size, A}) ->
  AI = I div ?W,
  V = array:get(AI, A),
  V1 = V bor (1 bsl (I rem ?W)),
  {?MODULE, Size, array:set(AI, V1, A)}.


%% -----------------------------------------------------------------------------
%% @doc Sets the value of the Nth bit to 0.
%% @end
%% -----------------------------------------------------------------------------
clear(I, {?MODULE, Size, A}) ->
  %% Determine the array index
  AI = I div ?W,
  %% Get the current value at the index
  V = array:get(AI, A),
  %% Create a mask to clear the bit
  Mask = bnot(1 bsl (I rem ?W)),
  %% Clear the bit using AND NOT
  V1 = V band Mask,
  %% Update the array with the new value
  {?MODULE, Size, array:set(AI, V1, A)}.


%% -----------------------------------------------------------------------------
%% @doc Flips the value of the Nth bit.
%% @end
%% -----------------------------------------------------------------------------
flip(I, {?MODULE, Size, A}) ->
  %% Determine the array index
  AI = I div ?W,
  %% Get the current value at the index
  V = array:get(AI, A),
  %% Create a mask for the target bit
  Mask = 1 bsl (I rem ?W),
  %% Toggle the bit using XOR
  V1 = V bxor Mask,
  %% Update the array with the new value
  {?MODULE, Size, array:set(AI, V1, A)}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(non_neg_integer(), t()) -> boolean().

get(I, {?MODULE, _, A}) ->
  AI = I div ?W,
  V = array:get(AI, A),
  V band (1 bsl (I rem ?W)) =/= 0.

