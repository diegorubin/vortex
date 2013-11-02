%% @author Diego Rubin <rubin.diego@gmail.com>
%% @copyright 2013 Diego Rubin 

%% @doc Functions which wrap up the communication with the Riak cluster
%%      plus a few other helper functions.

-module(vortex_core_riak).
-author('Diego Rubin <rubin.diego@gmail.com>').

%% Riak exports
-export([connect/0, connect/1, create/3, create/4, keys/2, fetch/3, update/2, get_value/1, save/2, delete/3]).

% Exported Functions
%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect() ->
  connect(vortex_core_riak_config:connection_info()).

%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect({IP, Port}) ->
  {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
  RiakPid.

%% @spec create(binary, binary, json) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      assumes that the data passed in is JSON and sets
%%      the MIME type to "application/json" for you.
create(Bucket, Key, JsonData) ->
  create(Bucket, Key, JsonData, "application/json").

%% @spec create(binary, binary, term(), string) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      takes arbitrary data and requires the user to
%%      specify the mime type of the data that is being
%%      stored.
create(Bucket, Key, Item, MimeType) ->
  RiakObj = riakc_obj:new(Bucket, Key, Item, MimeType),
  RiakObj.

%% @spec fetch(pid(), binary, binary) -> riakc_obj()
%% @doc Fetches a riakc object from a Riak node/cluster
%%      using the connection given.
fetch(RiakPid, Bucket, Key) ->
  RiakObj = riakc_pb_socket:get(RiakPid, Bucket, Key),
  RiakObj.

%% @spec keys(pid(), binary) -> list
%% @doc Fetches keys from a Riak bucket
%%      using the connection given.
keys(RiakPid, Bucket) ->
  List = riakc_pb_socket:list_keys(RiakPid, Bucket),
  List.

%% @spec update(riakc_obj(), term()) -> riakc_obj()
%% @doc Updates the stored value for a riakc object with
%%      the new one specified.
update(RiakObj, NewValue) ->
  NewRiakObj = riakc_obj:update_value(RiakObj, NewValue),
  NewRiakObj.

%% @spec delete(pid(), binary, binary) -> term()
%% @doc Destroy item of bucket
delete(RiakPid, Bucket, Key) ->
  Value = riakc_pb_socket:delete(RiakPid, Bucket, Key),
  Value.

%% @spec get_value(riakc_obj()) -> term()
%% @doc Retrieves the stored value from within the riakc
%%      object.
get_value(RiakObj) ->
  Value = riakc_obj:get_value(RiakObj),
  Value.

%% @spec save(pid(), riakc_obj()) -> {ok, riakc_obj()} | {error | Reason}
%% @doc Saves the given riak object to the specified Riak node/cluster.
save(RiakPid, RiakObj) ->
  Result = riakc_pb_socket:put(RiakPid, RiakObj),
  Result.

