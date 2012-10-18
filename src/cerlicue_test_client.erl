-module(cerlicue_test_client).
-behaviour(gen_server).

-record(s, {nick}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         nick/2,
         privmsg/3,
         join/2,
         part/3,
         quit/1]).

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

join(Pid, Channel) ->
    gen_server:call(Pid, {join, Channel}).

part(Pid, Channel, Msg) ->
    gen_server:call(Pid, {part, Channel, Msg}).

quit(Pid) ->
    gen_server:cast(Pid, quit).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #s{}}.

handle_call({nick, Nick}, _From, State) ->
    case cerlicue_server:nick(Nick, self()) of
        ok ->
            {reply, ok, State#s{nick=Nick}};
        {error, Code} ->
            {reply, {error, Code}, State}
    end;

handle_call({privmsg, Nick, Msg}, _From, State) ->
    Reply = cerlicue_server:privmsg(Nick, Msg, self()),
    {reply, Reply, State};

handle_call({join, Channel}, _From, State) ->
    Reply = cerlicue_server:join(Channel, self()),
    {reply, Reply, State};

handle_call({part, Channel, Msg}, _From, State) ->
    Reply = cerlicue_server:part(Channel, self(), Msg),
    {reply, Reply, State}.

handle_cast(quit, State) ->
    {stop, normal, State}.

handle_info({privmsg, Msg, SenderNick}, State) ->
    io:format("~p ~p~n", [SenderNick, Msg]),
    {noreply, State};

handle_info({forward, Msg, SenderNick, Channel}, State) ->
    io:format("~p> ~p ~p~n", [Channel, SenderNick, Msg]),
    {noreply, State};

handle_info({part, Msg, SenderNick, Channel}, State) ->
    io:format("~p> part ~p: ~p~n", [Channel, SenderNick, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
