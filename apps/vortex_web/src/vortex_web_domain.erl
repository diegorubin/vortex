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
  Domains = vortex_core_indexes:fetch_domains(),

  Content = [string:join(["{\"domain\": {\"name\": \"", Domain, "\", \"total\" : ", vortex_core_utils:int_to_string(vortex_core_indexes:total_pages(Domain))  , "}}"], "") || Domain <- Domains],

  Json = iolist_to_binary([
    "{\"domains\": [", 
     string:join(Content, ", "), 
    "]}"
  ]),

  {Json, ReqData, State}.

