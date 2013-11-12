-module(vortex_web_page).
-export([init/1,
         content_types_provided/2, 
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, State) ->
  Types = [
    {"application/json", to_json}
  ],
  {Types, ReqData, State}.

to_json(ReqData, State) ->
  PathInfo = wrq:path_info(ReqData),
  [{domain, Domain}] = PathInfo,

  Pages = vortex_core_indexes:fetch(Domain),
  Content = pages_to_list_of_links(Pages),

  Json = iolist_to_binary([
    "{\"pages\": [", 
     string:join(Content, "\", \""), 
    "]}"
  ]),

  {Json, ReqData, State}.

pages_to_list_of_links(Pages) ->
  pages_to_list_of_links(Pages, []).

pages_to_list_of_links([], List) ->
  List;
pages_to_list_of_links(Pages, List) ->
  [Head | Tail] = Pages,

  case vortex_core_page:fetch(Head) of
    {page,PageData} -> 
      [{key, _Key}, {domain, _Domain}, {title, Title}, {body, _Body}, {readat, _ReadAt}] = PageData,
      NewList = ["{\"title\":\"" ++ Title ++ "\", \"url\":\"" ++ Head ++ "\"}" |List],
      pages_to_list_of_links(Tail, NewList);
    notfound -> 
      pages_to_list_of_links(Tail, List)
  end.
  
