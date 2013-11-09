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
    Pid=supervisor:start_link({local, ?MODULE} , ?MODULE, []),
    {ok,Pid}.

init(_Args) ->
     io:format("ch1 has started (~w)~n", [self()]),
     % If the initialization is successful, the function
     % should return {ok,State}, {ok,State,Timeout} ..
     {ok, ch1State}.

