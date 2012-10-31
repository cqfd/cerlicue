-module(cerlicue_erl).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         nick/2,
         privmsg/3,
         join/2,
         topic/2,
         names/2,
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
privmsg(Pid, Name, Msg) ->
    gen_server:call(Pid, {privmsg, Name, Msg}).
join(Pid, Channel) ->
    gen_server:call(Pid, {join, Channel}).
topic(Pid, Channel) ->
    gen_server:call(Pid, {topic, Channel}).
names(Pid, Channel) ->
    gen_server:call(Pid, {names, Channel}).
part(Pid, Channel, Msg) ->
    gen_server:call(Pid, {part, Channel, Msg}).
quit(Pid) ->
    gen_server:cast(Pid, quit).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, []}.

handle_call({nick, Nick}, _From, State) ->
    Reply = cerlicue_backend:nick(Nick, self()),
    {reply, Reply, State};
handle_call({privmsg, Name, Msg}, _From, State) ->
    Reply = cerlicue_backend:privmsg(Name, Msg, self()),
    {reply, Reply, State};
handle_call({join, Channel}, _From, State) ->
    Reply = cerlicue_backend:join(Channel, self()),
    {reply, Reply, State};
handle_call({topic, Channel}, _From, State) ->
    Reply = cerlicue_backend:topic(Channel),
    {reply, Reply, State};
handle_call({names, Channel}, _From, State) ->
    Reply = cerlicue_backend:names(Channel),
    {reply, Reply, State};
handle_call({part, Channel, Msg}, _From, State) ->
    Reply = cerlicue_backend:part(Channel, Msg, self()),
    {reply, Reply, State}.

handle_cast(quit, State) ->
    {stop, normal, State}.

handle_info({nick, Old, New}, State) ->
    io:format("~p changed their nick to ~p~n", [Old, New]),
    {noreply, State};
handle_info({privmsg, From, To, Msg}, State) ->
    io:format("~p --> ~p: ~p~n", [From, To, Msg]),
    {noreply, State};
handle_info({join, Nick, Channel}, State) ->
    io:format("~p joined ~p~n", [Nick, Channel]),
    {noreply, State};
handle_info({part, Nick, Channel, Msg}, State) ->
    io:format("~p departed ~p: ~p~n", [Nick, Channel, Msg]),
    {noreply, State};
handle_info({quit, Nick, Msg}, State) ->
    io:format("~p quit: ~p~n", [Nick, Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
