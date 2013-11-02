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
%  Pages = vortex_core_page:all(),
%  Content = [vortex_core_json:to_json(Page) || Page <- Pages],

%  Json = iolist_to_binary([
%    "{\"pages\":", 
%    iolist_to_binary(Content), 
%    "}"
%  ]),

  {"{}", ReqData, State}.

