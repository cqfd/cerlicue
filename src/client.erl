-module(client).
-behaviour(gen_server).

-record(s, {nick}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, nick/2, privmsg/3, join/2, part/2, quit/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

nick(Pid, Nick) ->
    gen_server:call(Pid, {nick, Nick}).

privmsg(Pid, Nick, Msg) ->
    gen_server:call(Pid, {privmsg, Nick, Msg}).

join(Pid, ChannelNick) ->
    gen_server:call(Pid, {join, Pid, ChannelNick}).

part(Pid, ChannelNick) ->
    gen_server:call(Pid, {part, Pid, ChannelNick}).

quit(Pid) ->
    gen_server:call(Pid, quit).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #s{}}.

handle_call({nick, Nick}, _From, State) ->
    case cerlicue_router:register(Nick, self()) of
        ok ->
            {reply, ok, State#s{nick=Nick}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({privmsg, Nick, Msg}, _From, State) ->
    IrcMsg = {self(), 'PRIVMSG', [Nick], Msg},
    Reply = cerlicue_router:privmsg(Nick, IrcMsg),
    {reply, Reply, State};

handle_call(quit, _From, State) ->
    {stop, normal, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("~p received: ~p~n", [self(), _Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("About to terminate ~p~n", [self()]),
    case State#s.nick of
        undefined ->
            ok;
        Nick ->
            cerlicue_router:unregister(Nick),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
