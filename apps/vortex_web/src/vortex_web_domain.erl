-module(vortex_web_domain).
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
  Content = vortex_core_indexes:fetch_domains(),

  Json = iolist_to_binary([
    "{\"domains\": [\"", 
     string:join(Content, "\", \""), 
    "\"]}"
  ]),

  {Json, ReqData, State}.

