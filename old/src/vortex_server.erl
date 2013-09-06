-module(vortex_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_page/1, save_page/1]).

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

save_page(Page) ->
  gen_server:call(?SERVER, {save_page, Page}, infinity).

get_page(PageKey) ->
  gen_server:call(?SERVER, {get_page, PageKey}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ConnInfo]) ->
  {ok, ConnInfo}.

handle_call({save_page, Page}, _From, ConnInfo) ->
  RiakPid = vortex_riak:connect(ConnInfo),
  SavedPage = vortex_page:save(RiakPid, Page),
  {reply, SavedPage, ConnInfo};

handle_call({get_page, PageKey}, _From, ConnInfo) ->
  RiakPid = vortex_riak:connect(ConnInfo),
  Page = vortex_page:fetch(RiakPid, PageKey),
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


