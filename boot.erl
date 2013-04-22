-module(boot).

-export([start/0]).

start() ->
  application:start(vortex).
