%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin 

%% @doc Functions which wrap up the communication with the Riak cluster
%%      plus a few other helper functions.

-module(vortex_core_riak).
-author('Diego Rubin <rubin.diego@gmail.com>').

%% Riak exports
-export([connect/0, connect/1, create/3, create/4, fetch/2, update/2, get_value/1, save/1, delete/2]).

% Exported Functions
connect() ->
  connect(vortex_core_riak_config:connection_info()).

connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

create(Bucket, Key, JsonData) ->
  create(Bucket, Key, JsonData, "application/json").

create(Bucket, Key, Item, MimeType) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
  RiakObj.

fetch(Bucket, Key) ->
  RiakPid = connect(),
  RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
  riakc_pb_socket:stop(RiakPid),
  RiakObj.

update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

delete(Bucket, Key) ->
  RiakPid = connect(),
  Value = riakc_pb_socket:delete(RiakPid, Bucket, Key),
  riakc_pb_socket:stop(RiakPid),
  Value.

get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

save(RiakObj) ->
  RiakPid = connect(),
  Result = riakc_pb_socket:put(RiakPid, RiakObj),
  riakc_pb_socket:stop(RiakPid),
  Result.

