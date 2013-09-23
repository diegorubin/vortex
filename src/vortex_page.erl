-module(vortex_page).
-export([to_page/3, to_json/1, from_json/1, save/2, fetch/1, delete/1, url_to_key/1]).

-define(BUCKET, <<"pages">>).

to_page(Domain, Title, Body) ->
  {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
  ReadAt = vortex_json:to_json([{year, Year}, {month, Month}, {day, Day}, {hour, Hour},
                                {minute, Minute}, {second, Second}]),

  {page,
    [
      {domain, Domain},
      {title, Title},
      {body, Body},
      {readat, ReadAt}
    ]
  }.

to_json({page, PageData}) ->
  to_json_internal(PageData).

from_json(PageJson) ->
  from_json_internal(PageJson).

fetch(Key) ->
  RiakPid = vortex_riak:connect(),
  case vortex_riak:fetch(RiakPid, ?BUCKET, Key) of
  {ok, RiakObj} -> 
    PageJson = vortex_riak:get_value(RiakObj),
    from_json_internal(PageJson);
  {error, notfound} ->
      notfound
  end.

save(Page={page, PageData}, Url) ->
  RiakPid = vortex_riak:connect(),
  Key = url_to_key(Url),
  case fetch(Key) of
    notfound ->
      NewPageData = [{key, Key} | PageData],
      RiakObj = vortex_riak:create(?BUCKET, Key, to_json_internal(NewPageData)),
      ok = vortex_riak:save(RiakPid, RiakObj),
      {page, NewPageData};
    {page, _} ->
      RiakObj = vortex_riak:fetch(RiakPid, ?BUCKET, Key),
      NewRiakObj = vortex_riak:update(RiakObj, to_json_internal(PageData)),
      ok = vortex_riak:save(RiakPid, NewRiakObj),
      Page
  end.

delete(Url) ->
  Key = url_to_key(Url),
  RiakPid = vortex_riak:connect(),
  vortex_riak:delete(RiakPid, ?BUCKET, Key).


to_json_internal(PageData) ->
  vortex_json:to_json(PageData, fun is_string/1).

from_json_internal(PageJson) ->
  {page, vortex_json:from_json(PageJson, fun is_string/1)}.

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

