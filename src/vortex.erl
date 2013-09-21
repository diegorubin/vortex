%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc vortex startup code

-module(vortex).
-author('Diego Rubin <rubin.diego@gmail.com>').
-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the vortex server.
start() ->
  application:start(protobuffs),
  application:start(riak_pb),
  application:start(riakc),
  application:start(vortex).

%% @spec stop() -> ok
%% @doc Stop the vortex server.
stop() ->
  application:stop(protobuffs),
  application:stop(riak_pb),
  application:stop(riakc),
  application:stop(vortex).
