-module(vortex_web_page).
-export([init/1, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

to_json(ReqData, State) ->
  {Content, ReqData, State}.

