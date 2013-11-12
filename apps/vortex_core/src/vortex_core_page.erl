-module(vortex_core_page).
-export([to_page/3, all_of_domain/1, save/2, fetch/1, delete/1, url_to_key/1, find/2]).

-define(BUCKET, <<"pages">>).

to_page(Domain, Title, Body) ->
  {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
  ReadAt = vortex_core_json:to_json([{year, Year}, {month, Month}, {day, Day}, {hour, Hour},
                                {minute, Minute}, {second, Second}]),

  {page,
    [
      {domain, Domain},
      {title, unicode:characters_to_binary(Title)},
      {body, unicode:characters_to_binary(Body)},
      {readat, ReadAt}
    ]
  }.

all_of_domain(Domain) ->
  RiakPid = vortex_core_riak:connect(),
  Urls = vortex_core_indexes:fetch(Domain),
  Pages = [find(RiakPid, url_to_key(Url)) || Url <- Urls],
  lists:delete(notfound, Pages).

find(RiakPid, Key) -> 
  case vortex_core_riak:fetch(RiakPid, ?BUCKET, Key) of
  {ok, RiakObj} -> 
    PageJson = vortex_core_riak:get_value(RiakObj),
    from_json_internal(PageJson);
  {error, notfound} ->
      notfound
  end.

fetch(Url) ->
  RiakPid = vortex_core_riak:connect(),
  Key = url_to_key(Url),
  find(RiakPid, Key).

save(Page={page, PageData}, Url) ->
  RiakPid = vortex_core_riak:connect(),
  Key = url_to_key(Url),
  case fetch(Url) of
    notfound ->
      NewPageData = [{key, Key} | PageData],
      RiakObj = vortex_core_riak:create(?BUCKET, Key, to_json_internal(NewPageData)),
      ok = vortex_core_riak:save(RiakPid, RiakObj),

      vortex_core_indexes:add_page_in_domain_list(Url),

      {page, NewPageData};
    {page, _} ->
      {ok, RiakObj} = vortex_core_riak:fetch(RiakPid, ?BUCKET, Key),
      NewRiakObj = vortex_core_riak:update(RiakObj, to_json_internal(PageData)),
      ok = vortex_core_riak:save(RiakPid, NewRiakObj),
      Page
  end.

delete(Url) ->
  Key = url_to_key(Url),
  RiakPid = vortex_core_riak:connect(),
  vortex_core_riak:delete(RiakPid, ?BUCKET, Key).

to_json_internal(PageData) ->
  unicode:characters_to_binary(vortex_core_json:to_json(PageData, fun is_string/1)).

from_json_internal(PageJson) ->
  {page, vortex_core_json:from_json(unicode:characters_to_binary(PageJson), fun is_string/1)}.

is_string(domain) -> true;
is_string(title) -> true;
is_string(body) -> true;
is_string(readat) -> true;
is_string(_) -> false.

%% @spec url_to_key(list()) -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak using the given list parameter as the stuff
%%      to hash.
url_to_key(List) ->
  Hash = erlang:phash2(List),
  base64:encode(<<Hash:32>>).

