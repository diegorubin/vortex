-module(vortex_server).
-behaviour(spooky).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/1, post/2]).

%%
%% API Functions
%%

init([])->
  [{port, 9001}, {handlers, [?MODULE]}].

post(Req, [])->
  Args = Req:parse_post(),

  LinkPage = read_param(Args, "link_page"),

  Req:ok(io_lib:format("~s~n",[LinkPage])).

%%
%% Local Functions
%%
read_param([], _Name) ->
  "";
read_param(Args, Name) ->
  [Arg|Rest] = Args,

  case Arg of
    {Name, Value} ->
      Value;
    {_Name, _Value} ->
      read_param(Rest, Name)
  end.
  


