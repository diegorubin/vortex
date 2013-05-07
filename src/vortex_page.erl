-module(vortex_page).
-export([to_page/3, to_json/1, from_json/1, save/2, fetch/2]).

-define(BUCKET, <<"pages">>).

to_page(Uri, Title, Body) ->
  {page,
    [
      {uri, Uri},
      {title, Title},
      {body, Body}
    ]
  }.

to_json({page, PageData}) ->
  to_json_internal(PageData).

from_json(PageJson) ->
  from_json_internal(PageJson).

fetch(RiakPid, Key) ->
  {ok, RiakObj} = csd_riak:fetch(RiakPid, ?BUCKET, Key),
  PageJson = csd_riak:get_value(RiakObj),
  from_json_internal(PageJson).

save(RiakPid, Page={page, PageData}) ->
  case proplists:get_value(key, PageData, undefined) of
    undefined ->
      Key = csd_riak:new_key(),
      NewPageData = [{key, Key} | PageData],
      RiakObj = csd_riak:create(?BUCKET, Key, to_json_internal(NewPageData)),
      ok = csd_riak:save(RiakPid, RiakObj),
      {page, NewPageData};
    ExistingKey ->
      RiakObj = csd_riak:fetch(RiakPid, ?BUCKET, ExistingKey),
      NewRiakObj = csd_riak:update(RiakObj, to_json_internal(PageData)),
      ok = csd_riak:save(RiakPid, NewRiakObj),
      Page
  end.

to_json_internal(PageData) ->
  csd_json:to_json(PageData, fun is_string/1).

from_json_internal(PageJson) ->
  {page, csd_json:from_json(PageJson, fun is_string/1)}.

is_string(uri) -> true;
is_string(title) -> true;
is_string(body) -> true;
is_string(_) -> false.

