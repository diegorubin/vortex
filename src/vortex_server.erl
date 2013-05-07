-module(vortex_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_snippet/1, save_snippet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  ConnInfo = vortex_riak_config:connection_info(),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ConnInfo], []).

save_snippet(Page) ->
  gen_server:call(?SERVER, {save_snippet, Page}, infinity).

get_snippet(PageKey) ->
  gen_server:call(?SERVER, {get_snippet, PageKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ConnInfo]) ->
  {ok, ConnInfo}.

handle_call({save_snippet, Page}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  SavedPage = csd_snippet:save(RiakPid, Page),
  {reply, SavedPage, ConnInfo};

handle_call({get_snippet, PageKey}, _From, ConnInfo) ->
  RiakPid = csd_riak:connect(ConnInfo),
  Page = csd_snippet:fetch(RiakPid, PageKey),
  {reply, Page, ConnInfo};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


