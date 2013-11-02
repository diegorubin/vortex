%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex_core startup code

-module(vortex_core).
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
    ensure_started(mochiweb),
    vortex_core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the vortex_core server.
start() ->
  application:start(protobuffs),
  application:start(riak_pb),
  application:start(riakc),
  ensure_started(inets),
    ensure_started(crypto),
  ensure_started(mochiweb),
  application:start(vortex_core).

%% @spec stop() -> ok
%% @doc Stop the vortex_core server.
stop() ->
  Res= application:stop(vortex_core),
  application:stop(protobuffs),
  application:stop(riak_pb),
  application:stop(riakc),
  application:stop(mochiweb),
  application:stop(inets),
  Res.
