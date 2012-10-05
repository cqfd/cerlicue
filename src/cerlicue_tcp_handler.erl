-module(cerlicue_tcp_handler).
-behaviour(gen_server).

-define(PING_TIMEOUT, 10 * 1000).
-define(PING_INTERVAL, 10 * 1000).

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
    % Setting a timeout of 0 is an OTP trick. The init function is
    % invoked within gen_server:start_link and blocks its return; as
    % such, it may not be an appropriate place to do a lot of slow setup
    % Setting a timeout of 0 allows init to return immediately, while
    % giving you a chance to finish your setup within a handle_info
    % timeout handler.
    Timeout = 0,
    {ok, LSock, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, ok, State#s{idle=false}, ?PING_INTERVAL}.

handle_cast(_Msg, State) ->
    {noreply, State#s{idle=false}, ?PING_INTERVAL}.

% This is where we finish setting up a cerlicue_tcp_handler process
% aftering setting a timeout of 0 in init.
handle_info(timeout, LSock) when is_port(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    cerlicue_tcp_handler_sup:start_child(),
    {noreply, #s{sock=Sock, idle=false}, ?PING_INTERVAL};

% If the process hasn't received any messages in ?PING_INTERVAL, send a
% PING and mark the process as idle.
handle_info(timeout, State=#s{idle=false}) ->
    send_ping(State#s.sock),
    {noreply, State#s{idle=true}, ?PING_TIMEOUT};

% If the process times out while idle, our last PING went unanswered.
handle_info(timeout, State=#s{idle=true}) ->
    {stop, ping_timeout, State};

% This is where we read data off the client's socket.
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
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal IRC callbacks
%% ------------------------------------------------------------------

% Root function for handling a particular parsed IRC message. This
% server will invoke handle_irc_msg/2 on each IRC message it receives.
handle_irc_msg(State, {_P, "NICK", [Nick], _T}) ->
    io:format("Hi ~p!~n", [Nick]),
    {noreply, State};
handle_irc_msg(State, {_P, "USER", [_Nick, _Mode, _Unused], RealName}) ->
    io:format("Or rather, hi ~p!~n", [RealName]),
    {noreply, State};
handle_irc_msg(State, {_P, "PONG", _As, _T}) ->
    io:format("Thanks for the PONG!~n"),
    {noreply, State};
handle_irc_msg(State, {_Prefix, "QUIT", _Args, _Trail}) ->
    io:format("Peace out.~n"),
    {stop, irc_quit, State};
handle_irc_msg(State, IrcMsg) ->
    io:format("Handling: ~p~n", [IrcMsg]),
    {noreply, State}.

% Use handle_irc_msg/2 to a list of parsed IRC messages. Threads the
% server's state between calls to keep it updated.
statefully_handle(Parsings, State) ->
    case statefully_do(fun handle_irc_msg/2, State, Parsings) of
        {noreply, NewState} ->
            {noreply, NewState, ?PING_TIMEOUT};
        {stop, Reason, NewState} ->
            {stop, Reason, NewState}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% CrlfSeparatedMsgs -> {[Msgs], PartialMsg}
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

% statefully_do is an OTP-aware substitute for lists:foreach/2 that
% threads a state value to each invocation of F. F must return one of
% the conventional OTP return values.
statefully_do(_F, State, []) ->
    {noreply, State};
statefully_do(F, State, [X|Xs]) ->
    case F(State, X) of
        {stop, Reason, NewState} ->
            {stop, Reason, NewState};
        {noreply, NewState} ->
            statefully_do(F, NewState, Xs)
    end.

send_irc_msg(_Sock, error) ->
    ok;
send_irc_msg(Sock, IrcMsg) ->
    Unparsing = cerlicue_parser:unparse(IrcMsg),
    Msg = io_lib:format("~s~c~c", [Unparsing, $\r, $\n]),
    gen_tcp:send(Sock, Msg).

send_ping(Sock) ->
    send_irc_msg(Sock, {"", "PING", [], ""}).
