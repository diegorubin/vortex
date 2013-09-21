-module(vortex_page).
-export([to_page/3, to_json/1, from_json/1, save/2, fetch/2]).

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
  jiffy:encode(PageData).

from_json(PageJson) ->
  from_json_internal(PageJson).

fetch(RiakPid, Key) ->
  {ok, RiakObj} = vortex_riak:fetch(RiakPid, ?BUCKET, Key),
  PageJson = vortex_riak:get_value(RiakObj),
  from_json_internal(PageJson).

save(RiakPid, Page={page, PageData}) ->
  case proplists:get_value(key, PageData, undefined) of
    undefined ->
      Key = vortex_riak:new_key(),
      NewPageData = [{key, Key} | PageData],
      RiakObj = vortex_riak:create(?BUCKET, Key, to_json_internal(NewPageData)),
      ok = vortex_riak:save(RiakPid, RiakObj),
      {page, NewPageData};
    ExistingKey ->
      RiakObj = vortex_riak:fetch(RiakPid, ?BUCKET, ExistingKey),
      NewRiakObj = vortex_riak:update(RiakObj, to_json_internal(PageData)),
      ok = vortex_riak:save(RiakPid, NewRiakObj),
      Page
  end.

to_json_internal(PageData) ->
  vortex_json:to_json(PageData, fun is_string/1).

from_json_internal(PageJson) ->
  {page, vortex_json:from_json(PageJson, fun is_string/1)}.

is_string(domain) -> true;
is_string(title) -> true;
is_string(body) -> true;
is_string(readat) -> true;
is_string(_) -> false.

