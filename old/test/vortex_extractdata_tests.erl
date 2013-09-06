-module(vortex_extractdata_tests).

-include_lib("eunit/include/eunit.hrl").

%getpage_test_() ->
%  
%  [{"get accessible page",
%    fun() ->
%      meck:new(httpc),
%      meck:expect(httpc, request, fun() -> "<html></html>" end),
%      Result = vortex_extractdata:getpage("http://diegorubin.com"),
%      meck:unload(httpc),
%
%      ?assertEqual(
%        {ok, 
%          {statuscode, "200"},
%          {contenttype, ""},
%          {content, "<html></html>"}
%        }, 
%        Result)
%    end}].

getlinks_test_() ->
  
  [{"get links from page",
    fun() ->
      Result = vortex_extractdata:getlinks("http://diegorubin.com/admin"),

      ?assertEqual(
        [["http://diegorubin.com/admin/#"]],
        Result)
    end}].


