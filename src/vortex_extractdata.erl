-module(vortex_extractdata).
-author('rubin.diego@gmail.com').

-include_lib("eunit/include/eunit.hrl").

-export([getpage/1, getlinks/1, getlinks/2, getlinksfromdomains/2, search_for_links/1]).

getpage(Uri) ->
  inets:start(),

  KeyInet = list_to_atom(lists:flatten(io_lib:format("atom~p", [erlang:phash2(Uri)]))),

  {ok, Pid} = inets:start(httpc, [{profile, KeyInet}]),

  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(get, {Uri, []}, [], []),

  inets:stop(httpc, Pid),

  {ok, Body}.

getlinks(Uri) -> getlinks(Uri, []).

getlinks(Uri, Domains) ->

  UriWithDomains = put_domain_in_local_paths(getrawlinks(Uri), Uri),
  exclude_in_links(UriWithDomains, Domains).

getlinksfromdomains(Uri, Domains) ->

  UriWithDomains = put_domain_in_local_paths(getrawlinks(Uri), Uri),

  UriWithDomains -- exclude_in_links(UriWithDomains, Domains).

%
% Private functions
%

% - getrawlinks
getrawlinks(Uri) ->
  {ok, Page} = getpage(Uri),
  Result = re:run(Page,"<a.*?href=['\"](.*?)['\"].*?>",[global, {capture, [1], list}]),

  case Result of
    {match, Links} ->
      Links;
    _Else ->
      []
  end.

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

  case Result of
    {match, [Domain]} ->
      exclude_in_links(RestLinks, Domains, ResultLinks);
    _Else ->
      exclude_in_links(RestLinks, Domains, [Uri|ResultLinks])
  end.

% - put_domain_in_local_paths
put_domain_in_local_paths(Links, Uri) ->

  Result = re:run(Uri, "^(https?://[0-9a-zA-Z-.]+)/?",[{capture,[1],list}]),
  {match, [Domain]} = Result,

  put_domain_in_local_paths(Links, Domain, Uri, []).

put_domain_in_local_paths([], _Domain, _Uri, NewLinks) ->
  NewLinks;
put_domain_in_local_paths(Links, Domain, Uri, NewLinks) ->

  [Link|RestLinks] = Links,

  Result = re:run(Link, "^(/.*)",[{capture,[1],list}]),

  case Result of
    {match, Paths} ->
      [Path|_] = Paths,

        put_domain_in_local_paths(RestLinks, Domain, Uri, [[string:concat(Domain, Path)]|NewLinks]);

    _Else ->

      AbsolutePath = re:run(Link, "^(http|mailto|https|ftp)://",[{capture,[1],list}]),

      case AbsolutePath of 
        {match, _} ->
          put_domain_in_local_paths(RestLinks, Domain, Uri, [Link|NewLinks]);
        nomatch ->
          [Path|_] = Link,
          put_domain_in_local_paths(RestLinks, Domain, Uri, [[string:concat(Uri, string:concat("/", Path))]|NewLinks])
      end
  end.

% - search_for_links
search_for_links(Uri) ->
  {ok, File} = file:open(string:concat("uris/",Uri),write),
  file:open(File),
  % Verificar se existe a pagina no banco
  % Verificar se faz tempo que foi lida

  % logica de indexacao

  % abrir novo thread para cada link
  Links = getlinks(Uri),

  [spawn(fun() -> search_for_links(Link) end) || [Link] <- Links].

%
% tests
%
getpage_test_() ->
  _Body = getpage("http://google.com"),
  ok.

