-module(vortex_indexer_dwarf).
-author('rubin.diego@gmail.com').

-behaviour(gen_server).

-export([init/1, start_link/0, handle_cast/2]).

% not implemented
-export([terminate/2, handle_call/3, code_change/3, handle_info/2]).

init(_Args) ->
  {ok, []}.

start_link() ->
  gen_server:start_link(?MODULE, [], []).

handle_cast(_Arg, State) ->
  {stop, normal, State}.

handle_info(timeout, State) -> {stop, shutdown, State}.

handle_call(_Arg, _Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

