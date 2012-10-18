-module(cerlicue_tcp).
-behaviour(gen_server).

-define(PING_TIMEOUT, 10 * 1000).
-define(PING_INTERVAL, 1000 * 1000).

-record(s, {sock,
            nick,
            incomplete_msg="",
            idle=false}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

% Start and link to a new cerlicue_tcp_handler process. This will
% invoke the init callback.
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([LSock]) ->
    Timeout = 0,
    {ok, LSock, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, ok, State#s{idle=false}, ?PING_INTERVAL}.

handle_cast(_Msg, State) ->
    {noreply, State#s{idle=false}, ?PING_INTERVAL}.

handle_info(timeout, LSock) when is_port(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    cerlicue_tcp_sup:start_child(),
    {noreply, #s{sock=Sock, idle=false}, ?PING_INTERVAL};

handle_info(timeout, State=#s{idle=false}) ->
    send_ping(State#s.sock),
    {noreply, State#s{idle=true}, ?PING_TIMEOUT};

handle_info(timeout, State=#s{idle=true}) ->
    {stop, ping_timeout, State};

handle_info({tcp, _Sock, Data}, State=#s{incomplete_msg=PrevIncomplete}) ->
    case split_by_crlf(Data) of
        {[FirstMsg|RestMsgs], Partial} ->
            FirstCompleteMsg = PrevIncomplete ++ FirstMsg,
            CompleteMsgs = [FirstCompleteMsg|RestMsgs],
            Parsings = [cerlicue_parser:parse(M) || M <- CompleteMsgs],
            NewState = State#s{incomplete_msg=Partial, idle=false},
            statefully_handle(Parsings, NewState);
        {[], Partial} ->
            NewState = State#s{incomplete_msg=PrevIncomplete ++ Partial, idle=false},
            {noreply, NewState, ?PING_INTERVAL}
    end;

handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State};

handle_info({privmsg, Msg, SenderNick}, State=#s{nick=Nick,sock=Sock}) ->
    IrcMsg = {SenderNick, "PRIVMSG", [Nick], Msg},
    send_irc_msg(Sock, IrcMsg),
    {noreply, State};

handle_info({forward, Msg, SenderNick, Channel},
            State=#s{sock=Sock}) ->
    IrcMsg = {SenderNick, "PRIVMSG", [Channel], Msg},
    send_irc_msg(Sock, IrcMsg),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal IRC callbacks
%% ------------------------------------------------------------------

handle_irc_msg(State, {_P, "NICK", [Nick], _T}) ->
    case cerlicue_server:nick(Nick, self()) of
        ok ->
            {noreply, State#s{nick=Nick}};
        {error, Code} ->
            {stop, {error, Code}, State}
    end;

handle_irc_msg(State=#s{nick=Nick, sock=Sock},
               {_P, "USER", [Nick, _Mode, _Unused], _RealName}) ->
    send_001(Sock, Nick),
    {noreply, State};

handle_irc_msg(State=#s{nick=Nick, sock=Sock},
               {_P, "JOIN", [Channel], _T}) ->
    {ok, OtherNicks} = cerlicue_server:join(Channel, self()),
    send_join(Sock, Nick, Channel),
    send_353(Sock, Nick, Channel, OtherNicks),
    send_366(Sock, Nick, Channel),
    {noreply, State};

handle_irc_msg(State=#s{nick=Nick, sock=Sock},
               {_P, "MODE", [Channel], _T}) ->
    Mode = cerlicue_server:mode(Channel),
    send_324(Sock, Nick, Channel, Mode),
    {noreply, State};

handle_irc_msg(State=#s{nick=Nick, sock=Sock},
               {_P, "PRIVMSG", [Name], Msg}) ->
    case cerlicue_server:privmsg(Name, Msg, self()) of
        ok ->
            {noreply, State};
        {error, 401} ->
            send_401(Sock, Nick, Name),
            {noreply, State};
        {error, 403} ->
            send_403(Sock, Nick, Name),
            {noreply, State}
    end;

handle_irc_msg(State, {_P, "PONG", _As, _T}) ->
    {noreply, State};

handle_irc_msg(State,
               {_Prefix, "QUIT", _Args, _Trail}) ->
    {stop, irc_quit, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

split_by_crlf(CrlfSeparatedMsgs) ->
    split_by_crlf(CrlfSeparatedMsgs, "", []).

split_by_crlf("", ReversedPartial, Accumulated) ->
    Partial = lists:reverse(ReversedPartial),
    Messages = lists:reverse(Accumulated),
    {Messages, Partial};
split_by_crlf("\r\n" ++ Msg, ReversedPartial, Accumulated) ->
    Complete = lists:reverse(ReversedPartial),
    split_by_crlf(Msg, "", [Complete|Accumulated]);
split_by_crlf([C|Rest], ReversedPartial, Accumulated) ->
    split_by_crlf(Rest, [C|ReversedPartial], Accumulated).

statefully_handle(Parsings, State) ->
    case statefully_do(fun handle_irc_msg/2, State, Parsings) of
        {noreply, NewState} ->
            {noreply, NewState, ?PING_INTERVAL};
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.

statefully_do(_F, State, []) ->
    {noreply, State};
statefully_do(F, State, [X|Xs]) ->
    case F(State, X) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        {noreply, NewState} ->
            statefully_do(F, NewState, Xs)
    end.

send_ping(Sock) ->
    send_irc_msg(Sock, {"", "PING", [], ""}).

send_join(Sock, Nick, Channel) ->
    send_irc_msg(Sock, {Nick, "JOIN", [Channel], ""}).


send_001(Sock, Nick) ->
    send_irc_msg(Sock, {"cerlicue-local", "001", [Nick], "welcome to cerlicue!"}).

send_324(Sock, Nick, Channel, Mode) ->
    send_irc_msg(Sock, {"cerlicue-local", "324", [Nick, Channel, Mode], ""}).

send_353(Sock, Nick, Channel, OtherNicks) ->
    send_irc_msg(Sock, {"cerlicue-local",
                        "353",
                        [Nick, "=", Channel],
                        string:join([Nick|OtherNicks], " ")}).

send_366(Sock, Nick, Channel) ->
    send_irc_msg(Sock, {"cerlicue-local",
                        "366",
                        [Nick, Channel],
                        "End of /NAMES list"}).

send_401(Sock, Nick, NonExistentNick) ->
    IrcMsg = {"cerlicue-local", "401", [Nick, NonExistentNick], "No such nick"},
    send_irc_msg(Sock, IrcMsg).

send_403(Sock, Nick, NonExistentChannel) ->
    IrcMsg = {"cerlicue-local", "401", [Nick, NonExistentChannel], "No such nick"},
    send_irc_msg(Sock, IrcMsg).

send_irc_msg(_Sock, error) ->
    ok;
send_irc_msg(Sock, IrcMsg) ->
    Unparsing = cerlicue_parser:unparse(IrcMsg),
    Msg = io_lib:format("~s~c~c", [Unparsing, $\r, $\n]),
    gen_tcp:send(Sock, Msg).
