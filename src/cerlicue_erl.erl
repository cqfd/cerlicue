-module(cerlicue_erl).
-behaviour(gen_fsm).

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
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([awaiting_nick/3,
         connected/2,
         connected/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).
nick(Pid, Nick) ->
    gen_fsm:sync_send_event(Pid, {nick, Nick}).
privmsg(Pid, Name, Msg) ->
    gen_fsm:sync_send_event(Pid, {privmsg, Name, Msg}).
join(Pid, Channel) ->
    gen_fsm:sync_send_event(Pid, {join, Channel}).
topic(Pid, Channel) ->
    gen_fsm:sync_send_event(Pid, {topic, Channel}).
names(Pid, Channel) ->
    gen_fsm:sync_send_event(Pid, {names, Channel}).
part(Pid, Channel, Msg) ->
    gen_fsm:sync_send_event(Pid, {part, Channel, Msg}).
quit(Pid) ->
    gen_fsm:send_event(Pid, quit).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, awaiting_nick, []}.

awaiting_nick({nick, Nick}, _From, State) ->
    case cerlicue_backend:nick(Nick, self()) of
        ok ->
            {reply, ok, connected, State};
        {error, 432} ->
            {reply, {error, bad_nick}, awaiting_nick, State};
        {error, 433} ->
            {reply, {error, nick_taken}, awaiting_nick, State}
    end;
awaiting_nick(_Req, _From, State) ->
    {reply, {error, register_first}, awaiting_nick, State}.

connected({nick, Nick}, _From, State) ->
    Reply = cerlicue_backend:nick(Nick, self()),
    {reply, Reply, connected, State};
connected({privmsg, Name, Msg}, _From, State) ->
    Reply = cerlicue_backend:privmsg(Name, Msg, self()),
    {reply, Reply, connected, State};
connected({join, Channel}, _From, State) ->
    Reply = cerlicue_backend:join(Channel, self()),
    {reply, Reply, connected, State};
connected({topic, Channel}, _From, State) ->
    Reply = cerlicue_backend:topic(Channel),
    {reply, Reply, connected, State};
connected({names, Channel}, _From, State) ->
    Reply = cerlicue_backend:names(Channel),
    {reply, Reply, connected, State};
connected({part, Channel, Msg}, _From, State) ->
    Reply = cerlicue_backend:part(Channel, Msg, self()),
    {reply, Reply, connected, State}.

connected(quit, State) ->
    {stop, normal, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({nick, Old, New}, StateName, State) ->
    io:format("~p changed their nick to ~p~n", [Old, New]),
    {next_state, StateName, State};
handle_info({privmsg, From, To, Msg}, StateName, State) ->
    io:format("~p --> ~p: ~p~n", [From, To, Msg]),
    {next_state, StateName, State};
handle_info({join, Nick, Channel}, StateName, State) ->
    io:format("~p joined ~p~n", [Nick, Channel]),
    {next_state, StateName, State};
handle_info({part, Nick, Channel, Msg}, StateName, State) ->
    io:format("~p departed ~p: ~p~n", [Nick, Channel, Msg]),
    {next_state, StateName, State};
handle_info({quit, Nick, Msg}, StateName, State) ->
    io:format("~p quit: ~p~n", [Nick, Msg]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.
