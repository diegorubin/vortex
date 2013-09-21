%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex startup code

-module(vortex).
-author('Diego Rubin <rubin.diego@gmail.com>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the vortex server.
start() ->
    application:start(vortex).

%% @spec stop() -> ok
%% @doc Stop the vortex server.
stop() ->
    application:stop(vortex).
