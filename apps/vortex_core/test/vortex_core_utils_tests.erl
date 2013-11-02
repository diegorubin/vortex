%% -*- coding: utf-8 -*-
-module(vortex_core_utils_tests).

-include_lib("eunit/include/eunit.hrl").

remove_accent_test_() ->
  [{"remove accent of string",
    fun() -> 

      Result = vortex_core_utils:remove_accent("é pau, é pedra"),

      ?assertEqual("e pau, e pedra", Result)
    end}].

