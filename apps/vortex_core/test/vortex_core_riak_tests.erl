-module(vortex_core_riak_tests).

-include_lib("eunit/include/eunit.hrl").

keys_test_() ->
  [{"fetch all keys of bucket",
    fun() -> 
      {page, PageData} = vortex_core_page:to_page("http://diegorubin.com",
                                   "Página Pessoal", "<html></html>"),

      vortex_core_page:save({page, PageData}, "http://diegorubin.com/"),

      RiakPid = vortex_core_riak:connect(),
      Result = vortex_core_riak:keys(RiakPid, <<"pages">>),

      vortex_core_page:delete("http://diegorubin.com/"),
      ?assertEqual(["http://diegorubin.com/"], Result)
    end}].

