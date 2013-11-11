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

  Pages = vortex_core_page:all_of_domain(Domain),
  Content = [vortex_core_json:to_json(Page) || Page <- Pages],

  Json = iolist_to_binary([
    "{\"pages\": [", 
     string:join(Content, "\", \""), 
    "]}"
  ]),

  {Json, ReqData, State}.

