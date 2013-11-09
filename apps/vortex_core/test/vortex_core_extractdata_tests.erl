-module(vortex_core_extractdata_tests).

-include_lib("eunit/include/eunit.hrl").

getlinks_test_() ->
  
  [{"get links from page",
    fun() ->
      Result = vortex_core_extractdata:getlinks("http://arquivos.diegorubin.com/vortex_test.html"),

      ?assertEqual(
        [["http://arquivos.diegorubin.com/teste.jpg"],
         ["http://diegorubin.com"],
         ["http://arquivos.diegorubin.com/"]],
        Result)
    end}].

getlinks_and_exclude_domain_test_() ->
  
  [{"get links from page and exclude domain",
    fun() ->
      Result = vortex_core_extractdata:getlinks("http://arquivos.diegorubin.com/vortex_test.html", ["diegorubin.com"]),

      ?assertEqual(
        [["http://arquivos.diegorubin.com/"],
         ["http://arquivos.diegorubin.com/teste.jpg"]],
        Result)
    end}].

