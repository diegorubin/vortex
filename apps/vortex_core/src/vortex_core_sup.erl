%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin.

%% @doc Supervisor for the vortex_core application.

-module(vortex_core_sup).
-author('Diego Rubin <rubin.diego@gmail.com>').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  ets:new(ep, [named_table, public, set, {keypos, 1}]), 
  ets:insert(ep, {pids, 1}),
  RestartStrategy = {simple_one_for_one, 1000, 1},
  ChildSpec = {vortex_core_extractdata, {vortex_core_extractdata, start_link, []},
       permanent, 1000, worker, [vortex_core_extractdata]},
  Children = [ChildSpec],
  {ok, {RestartStrategy, Children}}.

