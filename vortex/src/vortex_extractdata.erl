-module(vortex_extractdata).
-author('rubin.diego@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([getpage/1, getlinks/1]).

getpage(Uri) ->
  inets:start(),
  {ok, Pid} = inets:start(httpc, [{profile, vortex_getpage}]),

  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(get, {Uri, []}, [], []),

  inets:stop(httpc, Pid),

  {ok, Body}.

getlinks(Uri) ->
  {ok, Page} = getpage(Uri),
  Result = re:run(Page,"<a.*?href=['\"](.*?)['\"].*?>",[global, {capture, [1], list}]),
  
  case Result of
    {match, Links} ->
      Links;
    nomatch ->
      [];
    _Else ->
      []
  end.

%
% tests
%
getpage_test_() ->
  Body = getpage("http://google.com"),
  ok.
