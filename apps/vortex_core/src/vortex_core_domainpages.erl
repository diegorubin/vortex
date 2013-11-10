%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex_core_domainpages startup code

-module(vortex_core_domainpages).
-author('Diego Rubin <rubin.diego@gmail.com>').

-export([add_page_in_domain_list/2, fetch/1, clear_index/1]).

-define(BUCKET, <<"domainpages">>).

add_page_in_domain_list(Domain, Page) ->
  RiakPid = vortex_core_riak:connect(),
  case fetch(Domain) of
    notfound ->
      RiakObj = vortex_core_riak:create(?BUCKET, Domain, [Page]),
      ok = vortex_core_riak:save(RiakPid, RiakObj),
      [Page];
    List ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, ?BUCKET, Domain),
      NewList = [Page | List],
      NewRiakObj = vortex_core_riak:update(RiakObj, NewList),
      ok = vortex_core_riak:save(RiakPid, NewRiakObj),
      NewList
  end.

fetch(Domain) ->
  RiakPid = vortex_core_riak:connect(),
  case vortex_core_riak:fetch(RiakPid, ?BUCKET, Domain) of
  {ok, RiakObj} -> 
    binary_to_term(vortex_core_riak:get_value(RiakObj));
  {error, notfound} ->
      notfound
  end.

clear_index(Domain) ->
  RiakPid = vortex_core_riak:connect(),
  case fetch(Domain) of
    notfound ->
      RiakObj = vortex_core_riak:create(?BUCKET, Domain, []),
      vortex_core_riak:save(RiakPid, RiakObj);
    _ ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, ?BUCKET, Domain),
      NewRiakObj = vortex_core_riak:update(RiakObj, []),
      vortex_core_riak:save(RiakPid, NewRiakObj)
  end.

