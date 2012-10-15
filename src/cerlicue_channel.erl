-module(cerlicue_channel).
-behaviour(gen_server).

-record(s, {nick, clients}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, join/2, names/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Nick) ->
    gen_server:start_link(?MODULE, [Nick], []).

join(ChannelPid, ClientPid) ->
    gen_server:call(ChannelPid, {join, ClientPid}).

names(ChannelPid) ->
    gen_server:call(ChannelPid, names).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Nick]) ->
    {ok, #s{nick=Nick, clients=[]}}.

handle_call({join, ClientPid}, _From, State=#s{clients=Clients}) ->
    {reply, ok, State#s{clients=[ClientPid|Clients]}};

handle_call(names, _From, State=#s{clients=Clients}) ->
    {reply, Clients, State}.

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
