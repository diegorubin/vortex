-module(vortex_extractdata).
-author('rubin.diego@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([getpage/1, getlinks/1, getlinks/2]).

getpage(Uri) ->
  inets:start(),
  {ok, Pid} = inets:start(httpc, [{profile, vortex_getpage}]),

  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(get, {Uri, []}, [], []),

  inets:stop(httpc, Pid),

  {ok, Body}.

getlinks(Uri) -> getlinks(Uri, []).

getlinks(Uri, Domains) ->
  {ok, Page} = getpage(Uri),
  Result = re:run(Page,"<a.*?href=['\"](.*?)['\"].*?>",[global, {capture, [1], list}]),
  

  UriWithDomains = case Result of
    {match, Links} ->
      put_domain_in_local_paths(Links, Uri);
    _Else ->
      []
  end,

  exclude_in_links(UriWithDomains, Domains).

%
% Private functions
%

% - exclude_in_links
exclude_in_links(Links, Domains) ->
  exclude_in_links(Links, Domains, []).

exclude_in_links(ResultLinks, [], []) ->

  ResultLinks;

exclude_in_links([], Domains, ResultLinks) ->

  [_|RestDomains] = Domains,

  exclude_in_links(ResultLinks, RestDomains, []);

exclude_in_links(Links, Domains, ResultLinks) ->
  [Domain|_] = Domains,

  [Uri|RestLinks] = Links,

  Result = re:run(Uri, "^https?://([0-9a-zA-Z-.]+)/?",[{capture,[1],list}]),
  {match, [LinkDomain]} = Result,

  case LinkDomain of
    Domain ->
      exclude_in_links(RestLinks, Domains, ResultLinks);
    _Else ->
      exclude_in_links(RestLinks, Domains, [Uri|ResultLinks])
  end.

% - put_domain_in_local_paths
put_domain_in_local_paths(Links, Uri) ->

  Result = re:run(Uri, "^(https?://[0-9a-zA-Z-.]+)/?",[{capture,[1],list}]),
  {match, [Domain]} = Result,

  put_domain_in_local_paths(Links, string:concat(Domain, "/"), []).

put_domain_in_local_paths([], _, NewLinks) ->
  NewLinks;
put_domain_in_local_paths(Links, Domain, NewLinks) ->

  [Link|RestLinks] = Links,

  Result = re:run(Link, "^(/.*)",[{capture,[1],list}]),

  case Result of
    {match, Paths} ->
      [Path|_] = Paths,
      put_domain_in_local_paths(RestLinks, Domain, [[string:concat(Domain, Path)]|NewLinks]);
    _Else ->
      put_domain_in_local_paths(RestLinks, Domain, [Link|NewLinks])
  end.

%
% tests
%
getpage_test_() ->
  _Body = getpage("http://google.com"),
  ok.

