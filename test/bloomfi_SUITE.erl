-module(bloomfi_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

%% List all test cases
all() ->
    [
        add_member_test,
        duplicate_add_test,
        memory_test,
        capacity_test
    ].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) ->
    ok.


%% add_member_test/1
%% Tests that an element is not a member until added, then is a member.
add_member_test(_Config) ->
    BF0 = bloomfi:new(100),
    ?assertEqual(false, bloomfi:member("test_element", BF0)),
    BF1 = bloomfi:add("test_element", BF0),
    ?assertEqual(true, bloomfi:member("test_element", BF1)).

%% duplicate_add_test/1
%% Ensures that adding the same element twice does not increment the size.
duplicate_add_test(_Config) ->
    BF0 = bloomfi:new(100),
    BF1 = bloomfi:add("dup_element", BF0),
    Size1 = bloomfi:size(BF1),
    BF2 = bloomfi:add("dup_element", BF1),
    Size2 = bloomfi:size(BF2),
    ?assertEqual(Size1, Size2).

%% memory_test/1
%% Verifies that the memory function returns a non-negative value.
memory_test(_Config) ->
    BF = bloomfi:new(100),
    Mem = bloomfi:memory(BF),
    ?assert(Mem >= 0).

%% capacity_test/1
%% Checks that the capacity reported is a positive integer.
capacity_test(_Config) ->
    BF = bloomfi:new(100),
    Cap = bloomfi:capacity(BF),
    ?assert(Cap > 0).