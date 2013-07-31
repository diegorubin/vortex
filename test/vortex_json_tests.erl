-module(vortex_json_tests).

-include_lib("eunit/include/eunit.hrl").

from_json_test_() ->
  [{"convert json to list",
    fun() ->
      Result = vortex_json:from_json("{\"config\": \"TESTE\"}"),

      ?assertEqual(
        [{config, "TESTE"}],
        Result)
    end}].

