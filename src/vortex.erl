%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex startup code

-module(vortex).
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
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    vortex_sup:start_link().

%% @spec start() -> ok
%% @doc Start the vortex server.
start() ->
  application:start(protobuffs),
  application:start(riak_pb),
  application:start(riakc),
  ensure_started(inets),
    ensure_started(crypto),
  ensure_started(mochiweb),
  application:set_env(webmachine, webmachine_logger_module, 
                      webmachine_logger),
  ensure_started(webmachine),
  application:start(vortex).

%% @spec stop() -> ok
%% @doc Stop the vortex server.
stop() ->
  Res= application:stop(vortex),
  application:stop(protobuffs),
  application:stop(riak_pb),
  application:stop(riakc),
  application:stop(webmachine),
  application:stop(mochiweb),
  application:stop(inets),
  Res.
