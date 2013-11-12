-module(vortex_web_watch).
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
  [{url, Url}] = PathInfo,

  vortex_core_extractdata:watch(Url),

  Json = iolist_to_binary([
    "{\"status\": \"ok\"}"
  ]),

  {Json, ReqData, State}.

