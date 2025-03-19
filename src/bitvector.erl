%% =============================================================================
%%  bit_vector.erl
%%  Original source from gist by Lukas Larsson
%%  https://gist.github.com/garazdawi/48f1284c0d533ab5a39eeac6f8ff99a0
%%
%%  Copyright (c) 2023 Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================


-module(bitvector).
-include("bloomfi.hrl").

?MODULEDOC("""
A bit vector implemented using atomics.
> Atomics are not tied to the current process and are automatically garbage
collected when they are no longer referenced.
""").
-record(?MODULE, {
    size            ::  pos_integer(),
    atomics_ref     ::  atomics:atomics_ref()
}).

-export([clear/2]).
-export([flip/2]).
-export([get/2]).
-export([intersection/2]).
-export([memory/1]).
-export([new/1]).
-export([print/1]).
-export([set/2]).
-export([size/1]).
-export([union/2]).



%% =============================================================================
%% API
%% =============================================================================


new(Size) ->
    Words = (Size + 63) div 64,
    #?MODULE{
        size = Size,
        atomics_ref = atomics:new(Words, [{signed, false}])
    }.


?DOC("""
Returns the size of the bit vector.
""").
size(#?MODULE{size = Size}) ->
    Size.


?DOC("""
Returns the value of the Nth bit.
""").
get(Bix, #?MODULE{atomics_ref = Aref}) ->
    Wix = (Bix div 64) + 1,
    Mask = (1 bsl (Bix rem 64)),

    case atomics:get(Aref, Wix) band Mask of
        0 -> false;
        Mask -> true
    end.


?DOC("""
Sets the value of the Nth bit to 1.
""").
set(Bix, #?MODULE{atomics_ref = Aref} = T) ->
    Mask = (1 bsl (Bix rem 64)),
    ok = update(Bix, Aref, fun(Word) -> Word bor Mask end),
    T.


?DOC("""
Sets the value of the Nth bit to 0.
""").
clear(Bix, #?MODULE{atomics_ref = Aref} = T) ->
    Mask = bnot (1 bsl (Bix rem 64)),
    ok = update(Bix, Aref, fun(Word) -> Word band Mask end),
    T.


?DOC("""
Flips the value of the Nth bit.
""").
flip(Bix, #?MODULE{atomics_ref = Aref} = T) ->
    Mask = (1 bsl (Bix rem 64)),
    ok = update(Bix, Aref, fun(Word) -> Word bxor Mask end),
    T.


?DOC("""
Prints the bit vector to the console
""").
print(#?MODULE{size = Size} = BV) ->
    print(BV, Size - 1).


?DOC("""
Returns a new bit vector that is the result of the logical or between
the provided bit vectors.
""").

union(#?MODULE{size = Size} = A, #?MODULE{size = Size} = B) ->
    union(A, B, new(Size), Size - 1);

union(_, _) ->
    error(badarg).


?DOC("""
Returns a new bit vector that is the result of the logical and between
the provided bit vectors.
""").

intersection(#?MODULE{size = Size} = A, #?MODULE{size = Size} = B) ->
    intersection(A, B, new(Size), Size - 1);

intersection(_, _) ->
    error(badarg).


memory(#?MODULE{atomics_ref = Aref}) ->
    #{memory := Mem} = atomics:info(Aref),
    Mem.


%% =============================================================================
%% PRIVATE
%% =============================================================================



update(Bix, Aref, Fun) ->
    Wix = (Bix div 64) + 1,
    update_loop(Wix, Aref, Fun, atomics:get(Aref, Wix)).

update_loop(Wix, Aref, Fun, Expected) ->
    case atomics:compare_exchange(Aref, Wix, Expected, Fun(Expected)) of
        ok ->
            ok;
        Was ->
            update_loop(Wix, Aref, Fun, Was)
    end.


union(A, B, C, 0 = Slot) ->
    Or = get(Slot, A) bor get(Slot, B),
    ok = maybe_set(Or, C, Slot),
    C;

union(A, B, C, Slot) ->
    Or = get(Slot, A) bor get(Slot, B),
    ok = maybe_set(Or, C, Slot),
    union(A, B, C, Slot - 1).


intersection(A, B, C, 0 = Slot) ->
    And = get(Slot, A) band get(Slot, B),
    ok = maybe_set(And, C, Slot),
    C;

intersection(A, B, C, Slot) ->
    And = get(Slot, A) band get(Slot, B),
    ok = maybe_set(And, C, Slot),
    intersection(A, B, C, Slot - 1).


maybe_set(1, C, Slot) -> set(Slot, C);
maybe_set(0, _, _) -> ok.


print(BV, 0) ->
    io:format("~B~n",[get(0, BV)]);

print(BV, Slot) ->
    io:format("~B",[get(Slot, BV)]),
    print(BV, Slot-1).