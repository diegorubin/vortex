-module(vortex_app).

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

start(_Type, _Args) ->
  ok.

stop(_State) ->
  ok.
