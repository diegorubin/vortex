-module(vortex_core_extractdata).
-author('rubin.diego@gmail.com').

-behaviour(gen_server).

-export([getlinks/1, getlinks/2, getpage/1]).
-export([init/1, start_link/0, handle_cast/2]).

% not implemented
-export([terminate/2, handle_call/3, code_change/3, handle_info/2]).

-define(REDOMAIN, "^https?://([0-9a-zA-Z-.]+)/?").

init(_Args) ->
  io:format("has started (~w)~n", [self()]),
  {ok, ch1State}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

handle_cast(Uri, State) ->
  getlinks(Uri),
  {stop, normal, State}.

handle_info(timeout, State) -> {stop, normal, State}.

handle_call(_Arg, _Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

% - getlinks
getlinks(Uri) -> getlinks(Uri, []).

getlinks(Uri, Domains) ->
  UriWithDomains = put_domain_in_local_paths(getrawlinks(Uri), Uri),
  exclude_in_links(UriWithDomains, Domains).

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

% - getrawlinks
getrawlinks(Uri) ->
  {ok, 
    {
      {statuscode, _StatusCode},
      {contenttype, _ContentType},
      {content, RawPage}
    }
  } = getpage(Uri),

  Page = vortex_core_utils:force_string_list(RawPage),

  case vortex_core_page:fetch(Uri) of
    notfound ->

      ResultTitle = re:run(Page, "<title.*?>(.*?)</title>", [notbol, {capture, [1], list}]),

      ResultDomain = re:run(Uri, ?REDOMAIN,[{capture,[1],list}]),

      RawDomain = case ResultDomain of
        {match, [D]} -> D;
        nomatch -> "naoencontrado"
      end,

      RawTitle = case ResultTitle of
        {match, [T]} -> T;
        nomatch -> "Sem titulo"
      end,

      Title = vortex_core_utils:force_string_list(RawTitle),
      Domain = vortex_core_utils:force_string_list(RawDomain),

      NewPage = vortex_core_page:to_page(Domain, Title, Page),
      vortex_core_page:save(NewPage, Uri),

      % Salvar imagem no banco
      % TODO: Remover isso assim q possivel e colocar no banco.
      save_images_in_page(Uri, Page);
    {page, _} ->

      % TODO: Verificar se faz tempo que foi lida
      
      ok
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
          put_domain_in_local_paths(RestLinks, Domain, Uri, [[string:concat(Domain, string:concat("/", Path))]|NewLinks])
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

