-module(cerlicue_router).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, register/2, unregister/1, join/2, privmsg/2, names/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register(Nick, Pid) ->
    gen_server:call(?SERVER, {register, Nick, Pid}).

join(Pid, Channel) ->
    gen_server:call(?SERVER, {join, Pid, Channel}).

privmsg(Nick, Msg) ->
    gen_server:call(?SERVER, {privmsg, Nick, Msg}).

% Unregister/quit. Removes the nick from the db. Quit may be a nicer
% name, matches the IRC protocol.
unregister(Nick) ->
    gen_server:cast(?SERVER, {unregister, Nick}).

names(ChannelNick) ->
    gen_server:call(?SERVER, {names, ChannelNick}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, dict:new()}.

% Callback for registering a nick.
handle_call({register, Nick, Pid}, _From, Map) ->
    case dict:find(Nick, Map) of
        {ok, Pid} ->
            {reply, ok, Map};
        {ok, _DifferentPid} ->
            {reply, {error, nick_taken}, Map};
        error ->
            NewMap = dict:store(Nick, Pid, Map),
            {reply, ok, NewMap}
    end;

handle_call({privmsg, Nick, Msg}, _From, Map) ->
    case dict:find(Nick, Map) of
        {ok, Pid} ->
            Pid ! Msg,
            {reply, ok, Map};
        error ->
            {reply, {error, nick_not_found}, Map}
    end;

handle_call({join, Pid, ChannelNick}, _From, Map) ->
    case dict:find(ChannelNick, Map) of
        {ok, ChannelPid} ->
            cerlicue_channel:join(ChannelPid, Pid),
            {reply, ok, Map};
        error ->
            {ok, ChannelPid} = cerlicue_channel_sup:start_child(ChannelNick),
            NewMap = dict:store(ChannelNick, ChannelPid, Map),
            cerlicue_channel:join(ChannelPid, Pid),
            {reply, ok, NewMap}
    end;

handle_call({names, ChannelNick}, _From, Map) ->
    case dict:find(ChannelNick, Map) of
        {ok, ChannelPid} ->
            {reply, cerlicue_channel:names(ChannelPid), Map};
        error ->
            {reply, error, Map}
    end.

% Callback for unregistering a nick from the router.
handle_cast({unregister, Nick}, Map) ->
    NewMap = dict:erase(Nick, Map),
    {noreply, NewMap}.
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
