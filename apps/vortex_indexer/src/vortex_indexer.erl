%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex_indexer startup code

-module(vortex_indexer).
-author('Diego Rubin <rubin.diego@gmail.com>').

-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    vortex_indexer_sup:start_link().

%% @spec start() -> ok
%% @doc Start the vortex_indexer server.
start() ->
  ensure_started(protobuffs),
  ensure_started(riak_pb),
  ensure_started(riakc),
  ensure_started(inets),
  ensure_started(crypto),
  application:start(vortex_indexer).

%% @spec stop() -> ok
%% @doc Stop the vortex_indexer server.
stop() ->
  Res= application:stop(vortex_indexer),
  application:stop(protobuffs),
  application:stop(riak_pb),
  application:stop(riakc),
  application:stop(mochiweb),
  application:stop(inets),
  Res.

