-module(vortex_riak_tests).

-include_lib("eunit/include/eunit.hrl").

%save_page_test_() ->
%  [{"save content of page in riak",
%    fun() -> 
%      Link = "http://arquivos.diegorubin.com/vortex_test.html",
%      Page = vortex_extractdata:getpage(Link),
%      Result = vortex_riak:save_page(Link, Page),
%
%      ?assertEqual({ok, new}, Result)
%    end}].
%
