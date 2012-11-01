-module(cerlicue_irc_consumer).
-behaviour(gen_fsm).

-define(HOST, "cerlicue.local").

-record(s, {sock, nick}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         nick/2,
         user/2,
         privmsg/3,
         join/2,
         part/3,
         topic/2,
         names/2,
         quit/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export([awaiting_nick/2,
         connected/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Sock) ->
    gen_fsm:start_link(?MODULE, [Sock], []).
nick(Pid, Nick) ->
    gen_fsm:send_event(Pid, {nick, Nick}).
user(Pid, Realname) ->
    gen_fsm:send_event(Pid, {user, Realname}).
privmsg(Pid, Name, Msg) ->
    gen_fsm:send_event(Pid, {privmsg, Name, Msg}).
join(Pid, Channel) ->
    gen_fsm:send_event(Pid, {join, Channel}),
    topic(Pid, Channel),
    names(Pid, Channel).
part(Pid, Channel, Msg) ->
    gen_fsm:send_event(Pid, {part, Channel, Msg}).
topic(Pid, Channel) ->
    gen_fsm:send_event(Pid, {topic, Channel}).
names(Pid, Channel) ->
    gen_fsm:send_event(Pid, {names, Channel}).
quit(Pid) ->
    gen_fsm:send_event(Pid, quit).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Sock]) ->
    {ok, awaiting_nick, #s{sock=Sock}}.

awaiting_nick({nick, Nick}, State=#s{sock=Sock}) ->
    case cerlicue_backend:nick(Nick, self()) of
        ok ->
            {next_state, connected, State#s{nick=Nick}};
        {error, 432} ->
            send_irc(Sock, ?HOST, "432", [Nick], "Nickname invalid"),
            {next_state, awaiting_nick, State};
        {error, 433} ->
            send_irc(Sock, ?HOST, "433", [Nick], "Nickname already in use"),
            {next_state, awaiting_nick, State}
    end;
awaiting_nick(_Req, State=#s{sock=Sock}) ->
    send_irc(Sock, ?HOST, "451", ["*"], "Register first!"),
    {next_state, awaiting_nick, State}.

connected({nick, Nick}, State=#s{sock=Sock}) ->
    case cerlicue_backend:nick(Nick, self()) of
        ok ->
            {next_state, connected, State#s{nick=Nick}};
        {error, 433} ->
            send_irc(Sock, ?HOST, "433", [Nick], "Nickname already in use"),
            {next_state, connected, State}
    end;
connected({user, Realname}, State=#s{sock=Sock, nick=Nick}) ->
    cerlicue_backend:user(Realname, self()),
    send_irc(Sock, ?HOST, "001", [Nick], "Welcome to cerlicue!"),
    {next_state, connected, State};
connected({privmsg, Name, Msg}, State=#s{sock=Sock, nick=Nick}) ->
    case cerlicue_backend:privmsg(Name, Msg, self()) of
        ok ->
            {next_state, connected, State};
        {error, 401} ->
            send_irc(Sock, ?HOST, "401", [Nick, Name], "No such nick!"),
            {next_state, connected, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Nick, Name], "No such channel!"),
            {next_state, connected, State}
    end;
connected({join, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    cerlicue_backend:join(Channel, self()),
    send_irc(Sock, Nick, "JOIN", [Channel], ""),
    {next_state, connected, State};
connected({part, Nick, Channel, Msg}, State=#s{sock=Sock}) ->
    case cerlicue_backend:part(Channel, Msg, self()) of
        ok ->
            send_irc(Sock, Nick, "PART", [Channel], Msg),
            {next_state, connected, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Nick], "No such channel!"),
            {next_state, connected, State};
        {error, 442} ->
            send_irc(Sock, ?HOST, "442", [Nick], "You're not on that channel!"),
            {next_state, connected, State}
    end;
connected({topic, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    case cerlicue_backend:topic(Channel) of
        {ok, Topic} ->
            send_irc(Sock, ?HOST, "332", [Nick, Channel], Topic),
            {next_state, connected, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Channel], "No such channel!"),
            {next_state, connected, State}
    end;
connected({names, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    case cerlicue_backend:names(Channel) of
        {ok, Names} ->
            send_irc(Sock, ?HOST, "353", [Nick, "=", Channel], string:join(Names, " ")),
            send_irc(Sock, ?HOST, "366", [Nick, Channel], "End of /NAMES list."),
            {next_state, connected, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Channel], "No such channel!"),
            {next_state, connected, State}
    end;
connected(quit, State) ->
    {stop, quit, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Request, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({nick, Old, New}, connected, State=#s{sock=Sock}) ->
    send_irc(Sock, Old, "NICK", [], New),
    {next_state, connected, State};
handle_info({privmsg, From, To, Msg}, connected, State=#s{sock=Sock}) ->
    send_irc(Sock, From, "PRIVMSG", [To], Msg),
    {next_state, connected, State};
handle_info({join, Nick, Channel}, connected, State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "JOIN", [Channel], ""),
    {next_state, connected, State};
handle_info({part, Nick, Channel, Msg}, connected, State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "PART", [Channel], Msg),
    {next_state, connected, State};
handle_info({quit, Nick, Msg}, connected, State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "QUIT", [], Msg),
    {next_state, connected, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_irc(Sock, Prefix, Command, Args, Trail) ->
    Msg = cerlicue_parser:unparse({Prefix, Command, Args, Trail}),
    gen_tcp:send(Sock, io_lib:format("~s~c~c", [Msg, $\r, $\n])).
