-module(vortex_core_page).
-export([to_page/3, all_of_domain/1, save/2, save_document/2, fetch/1, delete/1, url_to_key/1, find/1]).

-define(BUCKET, <<"pages">>).
-define(PAGEBUCKET, <<"rawpages">>).

to_page(Domain, Title, Url) ->
  {{Year, Month, Day},{Hour, Minute, Second}} = erlang:universaltime(),
  ReadAt = vortex_core_json:to_json([{year, Year}, {month, Month}, {day, Day}, {hour, Hour},
                                {minute, Minute}, {second, Second}]),

  {page,
    [
      {domain, Domain},
      {title, unicode:characters_to_binary(Title)},
      {url, Url},
      {readat, ReadAt}
    ]
  }.

all_of_domain(Domain) ->
  Urls = vortex_core_indexes:fetch(Domain),
  Pages = [find(url_to_key(Url)) || Url <- Urls],
  lists:delete(notfound, Pages).

find(Key) -> 
  case vortex_core_riak:fetch(?BUCKET, Key) of
  {ok, RiakObj} -> 
    PageJson = vortex_core_riak:get_value(RiakObj),
    from_json_internal(PageJson);
  {error, notfound} ->
      notfound
  end.

fetch(Url) ->
  Key = url_to_key(Url),
  find(Key).

save(Page={page, PageData}, Url) ->
  Key = url_to_key(Url),
  case fetch(Url) of
    notfound ->
      NewPageData = [{key, Key} | PageData],
      RiakObj = vortex_core_riak:create(?BUCKET, Key, to_json_internal(NewPageData)),
      ok = vortex_core_riak:save(RiakObj),

      vortex_core_indexes:add_page_in_domain_list(Url),

      {page, NewPageData};
    {page, _} ->
      {ok, RiakObj} = vortex_core_riak:fetch(?BUCKET, Key),
      NewRiakObj = vortex_core_riak:update(RiakObj, to_json_internal(PageData)),
      ok = vortex_core_riak:save(NewRiakObj),
      Page
  end.

save_document(Page, Url) ->
  Key = url_to_key(Url),
  case fetch(Url) of
    notfound ->
      RiakObj = vortex_core_riak:create(?PAGEBUCKET, Key, Page, "text/html"),
      ok = vortex_core_riak:save(RiakObj),
      Page;
     _ ->
      {ok, RiakObj} = vortex_core_riak:fetch(?PAGEBUCKET, Key),
      NewRiakObj = vortex_core_riak:update(RiakObj, Page),
      ok = vortex_core_riak:save(NewRiakObj),
      Page
  end.

delete(Url) ->
  Key = url_to_key(Url),
  vortex_core_riak:delete(?BUCKET, Key).

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

