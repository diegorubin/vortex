%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc Callbacks for the vortex_web application.

-module(vortex_web_app).
-author('Diego Rubin <rubin.diego@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for vortex_web.
start(_Type, _StartArgs) ->
    vortex_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for vortex_web.
stop(_State) ->
    ok.
