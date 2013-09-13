-module(vortex_tests).

-include_lib("eunit/include/eunit.hrl").

start_and_stop_test_() ->
  
  [{"start and stop server",
    fun() ->
      ResultStart = vortex:start(),
      ResultStop = vortex:stop(),

      ?assertEqual(ok, ResultStart),
      ?assertEqual(ok, ResultStop)
    end}].
