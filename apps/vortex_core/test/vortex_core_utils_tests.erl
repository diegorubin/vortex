%% -*- coding: utf-8 -*-
-module(vortex_core_utils_tests).

-include_lib("eunit/include/eunit.hrl").

remove_accent_test_() ->
  [{"remove accent of string",
    fun() -> 

      Result = vortex_core_utils:remove_accent("é pau, é pedra"),

      ?assertEqual("e pau, e pedra", Result)
    end}].

in_list_test_() ->
  [{"check if item is in list",
    fun() ->
      Result = vortex_core_utils:in_list([a, b], b),
      ?assertEqual(true, Result)
    end}].

in_list_2_test_() ->
  [{"check if item is in list",
    fun() ->
      Result = vortex_core_utils:in_list([a, b], c),
      ?assertEqual(false, Result)
    end}].

put_on_list_if_not_have_test_() ->
  [{"put element on list if have not",
    fun() ->
      Result = vortex_core_utils:put_on_list_if_not_have([a, b], b),
      ?assertEqual([a, b], Result)
    end}].

put_on_list_if_not_have_2_test_() ->
  [{"not put element on list if have",
    fun() ->
      Result = vortex_core_utils:put_on_list_if_not_have([a, b], c),
      ?assertEqual([c, a, b], Result)
    end}].

