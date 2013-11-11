%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex_core_indexes startup code

-module(vortex_core_indexes).
-author('Diego Rubin <rubin.diego@gmail.com>').

-export([add_page_in_domain_list/1, add_page_in_domain_list/2, fetch/1, fetch/2, clear_index/1, clear_index/2, fetch_domains/0, total_pages/1]).

-define(PAGESLIST, <<"domainpages">>).
-define(DOMAINS, <<"domains">>).
-define(DOMAINLIST, <<"vortexdomainlist">>).

add_domain_in_list(Domain) -> 
  RiakPid = vortex_core_riak:connect(),

  case fetch(?DOMAINS, ?DOMAINLIST) of
    notfound ->
      RiakObj = vortex_core_riak:create(?DOMAINS, ?DOMAINLIST, [Domain]),
      ok = vortex_core_riak:save(RiakPid, RiakObj),
      [Domain];
    List ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, ?DOMAINS, ?DOMAINLIST),
      NewList = vortex_core_utils:put_on_list_if_not_have(List, Domain),
      NewRiakObj = vortex_core_riak:update(RiakObj, NewList),
      ok = vortex_core_riak:save(RiakPid, NewRiakObj),
      NewList
  end.

add_page_in_domain_list(Url) ->
  Result = re:run(Url, "^https?://([0-9a-zA-Z-.]+)/?",[{capture,[1],list}]),

  case Result of
    {match, [Domain]} ->
      add_page_in_domain_list(Domain, Url);
    _Else ->
      add_page_in_domain_list(Url, Url)
  end.

add_page_in_domain_list(Domain, Page) ->
  RiakPid = vortex_core_riak:connect(),

  add_domain_in_list(Domain),

  case fetch(Domain) of
    notfound ->
      RiakObj = vortex_core_riak:create(?PAGESLIST, Domain, [Page]),
      ok = vortex_core_riak:save(RiakPid, RiakObj),
      [Page];
    List ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, ?PAGESLIST, Domain),
      NewList = vortex_core_utils:put_on_list_if_not_have(List, Page),
      NewRiakObj = vortex_core_riak:update(RiakObj, NewList),
      ok = vortex_core_riak:save(RiakPid, NewRiakObj),
      NewList
  end.

fetch_domains() -> 
  fetch(?DOMAINS, ?DOMAINLIST).

fetch(Domain) ->
  fetch(?PAGESLIST, Domain).

fetch(Bucket, Key) ->
  RiakPid = vortex_core_riak:connect(),
  case vortex_core_riak:fetch(RiakPid, Bucket, Key) of
  {ok, RiakObj} -> 
    binary_to_term(vortex_core_riak:get_value(RiakObj));
  {error, notfound} ->
      notfound
  end.

total_pages(Domain) ->
  vortex_core_utils:list_len(fetch(Domain)).
  
clear_index(Domain) ->
  clear_index(?PAGESLIST, Domain).

clear_index(Bucket, Key) ->
  RiakPid = vortex_core_riak:connect(),
  case fetch(Bucket, Key) of
    notfound ->
      RiakObj = vortex_core_riak:create(Bucket, Key, []),
      vortex_core_riak:save(RiakPid, RiakObj);
    _ ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, Bucket, Key),
      NewRiakObj = vortex_core_riak:update(RiakObj, []),
      vortex_core_riak:save(RiakPid, NewRiakObj)
  end.

