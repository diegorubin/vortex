%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex startup code

-module(vortex).
-author('Diego Rubin <rubin.diego@gmail.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
%    vortex_sup:start_link().
    ok.

%% @spec start() -> ok
%% @doc Start the vortex server.
start() ->
    application:start(vortex).

%% @spec stop() -> ok
%% @doc Stop the vortex server.
stop() ->
    application:stop(vortex).
