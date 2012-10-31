-module(cerlicue_irc_consumer).
-behaviour(gen_server).

-define(HOST, "cerlicue.local").

-record(s, {sock, nick}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         nick/2,
         user/4,
         privmsg/3,
         join/2,
         part/3,
         topic/2,
         names/2,
         quit/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

nick(Pid, Nick) ->
    gen_server:cast(Pid, {nick, Nick}).

user(Pid, Nick, Mode, Realname) ->
    gen_server:cast(Pid, {user, Nick, Mode, Realname}).

privmsg(Pid, Name, Msg) ->
    gen_server:cast(Pid, {privmsg, Name, Msg}).

join(Pid, Channel) ->
    gen_server:cast(Pid, {join, Channel}),
    topic(Pid, Channel),
    names(Pid, Channel).

part(Pid, Channel, Msg) ->
    gen_server:cast(Pid, {part, Channel, Msg}).

topic(Pid, Channel) ->
    gen_server:cast(Pid, {topic, Channel}).

names(Pid, Channel) ->
    gen_server:cast(Pid, {names, Channel}).

quit(Pid) ->
    gen_server:cast(Pid, quit).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Sock]) ->
    {ok, #s{sock=Sock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({nick, Nick}, State=#s{sock=Sock}) ->
    case cerlicue_backend:nick(Nick, self()) of
        ok ->
            {noreply, State#s{nick=Nick}};
        {error, 433} ->
            send_irc(Sock, ?HOST, "433", [Nick], "Nickname already in use"),
            {noreply, State}
    end;
handle_cast({user, Nick, _Mode, _Realname}, State=#s{sock=Sock}) ->
    send_irc(Sock, ?HOST, "001", [Nick], "Welcome to cerlicue!"),
    {noreply, State};
handle_cast({privmsg, Name, Msg}, State=#s{sock=Sock}) ->
    case cerlicue_backend:privmsg(Name, Msg, self()) of
        ok ->
            {noreply, State};
        {error, 401} ->
            send_irc(Sock, ?HOST, "401", [Name], "No such nick!"),
            {noreply, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Name], "No such channel!"),
            {noreply, State}
    end;
handle_cast({join, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    cerlicue_backend:join(Channel, self()),
    send_irc(Sock, Nick, "JOIN", [Channel], ""),
    {noreply, State};
handle_cast({topic, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    case cerlicue_backend:topic(Channel) of
        {ok, Topic} ->
            send_irc(Sock, ?HOST, "332", [Nick, Channel], Topic),
            {noreply, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Channel], "No such channel!"),
            {noreply, State}
    end;
handle_cast({names, Channel}, State=#s{sock=Sock, nick=Nick}) ->
    case cerlicue_backend:names(Channel) of
        {ok, Names} ->
            send_irc(Sock, ?HOST, "353", [Nick, "=", Channel], string:join(Names, " ")),
            send_irc(Sock, ?HOST, "366", [Nick, Channel], "End of /NAMES list."),
            {noreply, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Channel], "No such channel!"),
            {noreply, State}
    end;
handle_cast({part, Nick, Channel, Msg}, State=#s{sock=Sock}) ->
    case cerlicue_backend:part(Channel, Msg, self()) of
        ok ->
            send_irc(Sock, Nick, "PART", [Channel], Msg),
            {noreply, State};
        {error, 403} ->
            send_irc(Sock, ?HOST, "403", [Nick], "No such channel!"),
            {noreply, State};
        {error, 442} ->
            send_irc(Sock, ?HOST, "442", [Nick], "You're not on that channel!"),
            {noreply, State}
    end;
handle_cast(quit, State) ->
    {stop, quit, State}.

handle_info({nick, Old, New}, State=#s{sock=Sock}) ->
    send_irc(Sock, Old, "NICK", [], New),
    {noreply, State};
handle_info({privmsg, From, To, Msg}, State=#s{sock=Sock}) ->
    send_irc(Sock, From, "PRIVMSG", [To], Msg),
    {noreply, State};
handle_info({join, Nick, Channel}, State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "JOIN", [Channel], ""),
    {noreply, State};
handle_info({part, Nick, Channel, Msg}, State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "PART", [Channel], Msg),
    {noreply, State};
handle_info({quit, Nick, Msg},State=#s{sock=Sock}) ->
    send_irc(Sock, Nick, "QUIT", [], Msg),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_irc(Sock, Prefix, Command, Args, Trail) ->
    Msg = cerlicue_parser:unparse({Prefix, Command, Args, Trail}),
    gen_tcp:send(Sock, io_lib:format("~s~c~c", [Msg, $\r, $\n])).
