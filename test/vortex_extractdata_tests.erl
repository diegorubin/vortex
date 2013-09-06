-module(vortex_extractdata_tests).

-include_lib("eunit/include/eunit.hrl").

getlinks_test_() ->
  
  [{"get links from page",
    fun() ->
      Result = vortex_extractdata:getlinks("http://diegorubin.com/admin"),

      ?assertEqual(
        [["http://diegorubin.com/admin/#"]],
        Result)
    end}].


