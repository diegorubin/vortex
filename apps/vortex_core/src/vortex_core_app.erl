%% @author Diego Rubin <rubin.diego@gmail.com>

%% @doc Callbacks for the vortex_core application.

-module(vortex_core_app).
-author('Diego Rubin <rubin.diego@gmail.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for vortex_core.
start(_Type, _StartArgs) ->
    vortex_core_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for vortex_core.
stop(_State) ->
    ok.

