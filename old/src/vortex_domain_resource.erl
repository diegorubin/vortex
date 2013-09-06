-module(vortex_domain_resource).
-author('rubin.diego@gmail.com').

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->

    
    {"<html><body>Todos os links</body></html>", ReqData, State}.


