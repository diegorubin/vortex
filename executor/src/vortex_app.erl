-module(vortex_app).

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

start(_Type, _Args) ->
  spooky:start_link(vortex_server).

stop(_State) ->
  spooky:stop().
