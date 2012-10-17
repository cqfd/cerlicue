-module(cerlicue_channel).
-behaviour(gen_server).

-record(s, {nick, clients}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, join/2, names/1, privmsg/3]).

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

join(ChannelPid, ClientPid) when is_pid(ChannelPid) ->
    gen_server:call(ChannelPid, {join, ClientPid});
join(ChannelNick, ClientPid) ->
    {ok, ChannelPid} = cerlicue_router:get_pid(ChannelNick),
    join(ChannelPid, ClientPid).

names(ChannelPid) when is_pid(ChannelPid) ->
    gen_server:call(ChannelPid, names);
names(ChannelNick) ->
    {ok, ChannelPid} = cerlicue_router:get_pid(ChannelNick),
    names(ChannelPid).

privmsg(ChannelPid, Msg, FromPid) when is_pid(ChannelPid) ->
    gen_server:call(ChannelPid, {privmsg, Msg, FromPid});
privmsg(ChannelNick, Msg, FromPid) ->
    {ok, ChannelPid} = cerlicue_router:get_pid(ChannelNick),
    privmsg(ChannelPid, Msg, FromPid).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Nick]) ->
    {ok, #s{nick=Nick, clients=[]}}.

handle_call({join, ClientPid}, _From, State=#s{clients=Clients}) ->
    {reply, ok, State#s{clients=[ClientPid|Clients]}};

handle_call(names, _From, State=#s{clients=Clients}) ->
    {reply, Clients, State};

handle_call({privmsg, Msg, FromPid}, _From, State=#s{clients=Clients}) ->
    % TODO: check is sender is in the channel.
    io:format("Trying to send a privmsg to a channel!~n"),
    lists:foreach(fun(ClientPid) ->
                          case ClientPid =:= FromPid of
                              true ->
                                  io:format("Not sending to self! ~p~n", [Clients]),
                                  ok;
                              false ->
                                  io:format("About to send a message! ~p~n", [Clients]),
                                  ClientPid ! {you_got_mail, Msg}
                          end
                  end,
                  Clients),
    {reply, ok, State}.

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
