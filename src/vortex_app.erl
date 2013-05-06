%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the vortex application.

-module(vortex_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for vortex.
start(_Type, _StartArgs) ->
    vortex_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for vortex.
stop(_State) ->
    ok.
