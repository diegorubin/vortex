-module(vortex_extractdata_tests).

-include_lib("eunit/include/eunit.hrl").

increment(N) -> N + 1.

getpage_test() ->
  [{"inc by 0",
    fun () ->
        ?assertEqual(1, increment(0))
    end},
   {"inc by 1",
    ?_test(?assertEqual(2, increment(1)))}].



