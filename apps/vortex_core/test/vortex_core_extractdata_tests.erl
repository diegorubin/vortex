-module(vortex_core_extractdata_tests).

-include_lib("eunit/include/eunit.hrl").

getlinks_test_() ->
  
  [{"get links from page",
    fun() ->
      Uri = "http://arquivos.diegorubin.com/vortex_test.html",

      inets:start(),
      KeyInet = list_to_atom(lists:flatten(io_lib:format("atom~p", [erlang:phash2(Uri)]))),
      {ok, Pid} = inets:start(httpc, [{profile, KeyInet}]),

      Result = vortex_core_extractdata:getlinks(Uri),

      inets:stop(httpc, Pid),

      ?assertEqual(
        [["http://arquivos.diegorubin.com/teste.jpg"],
         ["http://diegorubin.com"],
         ["http://arquivos.diegorubin.com/"]],
        Result)
    end}].

getlinks_and_exclude_domain_test_() ->
  
  [{"get links from page and exclude domain",
    fun() ->
      Uri = "http://arquivos.diegorubin.com/vortex_test.html",

      inets:start(),
      KeyInet = list_to_atom(lists:flatten(io_lib:format("atom~p", [erlang:phash2(Uri)]))),
      {ok, Pid} = inets:start(httpc, [{profile, KeyInet}]),

      Result = vortex_core_extractdata:getlinks(Uri, ["diegorubin.com"]),

      inets:stop(httpc, Pid),

      ?assertEqual(
        [["http://arquivos.diegorubin.com/"],
         ["http://arquivos.diegorubin.com/teste.jpg"]],
        Result)
    end}].

