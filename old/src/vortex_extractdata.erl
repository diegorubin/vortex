-module(vortex_extractdata).
-author('rubin.diego@gmail.com').

-export([getpage/1, getlinks/1, getlinks/2, getlinksfromdomains/2,
         search_for_links/1, search_for_links_in_domains/2,
         save_images_in_page/2]).

% - getpage
getpage(Uri) ->
  inets:start(),

  KeyInet = list_to_atom(lists:flatten(io_lib:format("atom~p", [erlang:phash2(Uri)]))),

  {ok, Pid} = inets:start(httpc, [{profile, KeyInet}]),

  {ok, {{_Version, StatusCode, _ReasonPhrase}, Headers, Content}} =
    httpc:request(get, {Uri, []}, [], []),

  [ContentType|_] = [CT || {"content-type", CT} <- Headers],

  inets:stop(httpc, Pid),

  {ok, 
    {
      {statuscode, StatusCode}, 
      {contenttype, ContentType}, 
      {content, Content}
    }
  }.

% - getlinks
getlinks(Uri) -> getlinks(Uri, []).

getlinks(Uri, Domains) ->

  UriWithDomains = put_domain_in_local_paths(getrawlinks(Uri), Uri),
  exclude_in_links(UriWithDomains, Domains).

% - getlinksfromdomains
getlinksfromdomains(Uri, Domains) ->

  UriWithDomains = put_domain_in_local_paths(getrawlinks(Uri), Uri),

  UriWithDomains -- exclude_in_links(UriWithDomains, Domains).

% - search_for_links
search_for_links(Uri) ->
  FileName = lists:flatten([C || C <- Uri, C /= $/, C /= $:, C /= $#]),
  Result = file:open(FileName, [read]),

  case Result of
    {error, enoent} ->
      Links = getlinks(Uri),

      [spawn(fun() -> search_for_links(Link) end) || [Link] <- Links];
    {ok, FileId} ->
      file:close(FileId)
  end.

% - search_for_links_in_domains
search_for_links_in_domains(Uri, Domains) ->
  FileName = lists:flatten([C || C <- Uri, C /= $/, C /= $:]),
  Result = file:open(FileName, [read]),

  case Result of
    {error, enoent} ->
      Links = getlinksfromdomains(Uri, Domains),

      [spawn(fun() -> search_for_links_in_domains(Link, Domains) end) || [Link] <- Links];
    {ok, FileId} ->
      file:close(FileId)
  end.

%
% Private functions
%

% - getrawlinks
getrawlinks(Uri) ->
  {ok, 
    {
      {statuscode, _StatusCode},
      {contenttype, _ContentType},
      {content, Page}
    }
  } = getpage(Uri),

  % Sera substituido pela chave do banco
  FileName = lists:flatten([C || C <- Uri, C /= $/, C /= $:, C /= $#]),
  OpenResult = file:open(FileName, [read]),

  case OpenResult of
    {error, enoent} ->

      % Substitutir para salvar no banco
      {ok, File} = file:open(FileName, write),
      io:format(File, "~s", [Page]),
      file:close(File),
      % Verificar se faz tempo que foi lida

      % logica de indexacao

      % abrir novo thread para cada link

      % Salvar imagem no banco
      % Remover isso assim q possivel e colocar no banco.
      save_images_in_page(Uri, Page);
    {ok, FileId} ->
      file:close(FileId)
  end,

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

% - save_images_in_page 
save_images_in_page(Uri, Page) ->

  Result = re:run(Page,"<img.*?src=['\"](.*?)['\"].*?>",[global, {capture, [1], list}]),

  Images = case Result of
    {match, Links} ->
      Links,
      put_domain_in_local_paths(Links, Uri);
    _Else ->
      []
  end,

  [save_image(Image) || [Image] <- Images].

save_image(Image) ->
  {ok, 
    {
      {statuscode, _StatusCode},
      {contenttype, _ContentType},
      {content, Content}
    }
  } = getpage(Image),

  FileName = case re:run(Image, "/([^/?]+)[\\?$]", [global, {capture, [1], list}]) of
    {match, [[X]]} -> X;
    nomatch -> lists:flatten(io_lib:format("~p.jpg", [erlang:phash2(Image)]))
  end,

  case file:open(FileName, [write, binary]) of
    {ok, File} ->
      file:write(File, Content),
      file:close(File);
    _Else ->
      nosave
  end.
