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

  TotalPages = lists:sum([vortex_core_indexes:total_pages(Domain) || Domain <- Domains]),

  Content = [string:join(["{\"domain\": {\"name\": \"", Domain, "\", \"total\" : ", vortex_core_utils:int_to_string(vortex_core_indexes:total_pages(Domain))  , "}}"], "") || Domain <- Domains],

  Json = iolist_to_binary([
    "{\"domains\": [", 
     string:join(Content, ", "), 
    "],",
    "\"total_domains\":",
    total_domains(Content), 
    ", \"total_pages\":",
    vortex_core_utils:int_to_string(TotalPages),
    "}"
  ]),

  {Json, ReqData, State}.

total_domains(Content) ->
  vortex_core_utils:int_to_string(total_domains(Content, 0)).

total_domains([], Total) -> Total;
total_domains(Content, Total) ->
  [_|Rest] = Content,
  NewTotal = Total + 1,
  total_domains(Rest, NewTotal).

