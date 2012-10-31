-module(cerlicue_irc_producer).
-behaviour(gen_server).

-record(s, {sup, sock, peer, incomplete_msg=""}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

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
start_link(Sup, LSock) ->
    gen_server:start_link(?MODULE, [Sup, LSock], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Sup, LSock]) ->
    Timeout = 0,
    {ok, #s{sup=Sup, sock=LSock}, Timeout}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #s{sup=Sup, sock=LSock}) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Peer} = cerlicue_irc_server_sup:start_consumer(Sup, Sock),
    cerlicue_irc_sup:start_child(),
    {noreply, #s{sock=Sock, peer=Peer}};

handle_info({tcp, _Sock, Data}, State=#s{incomplete_msg=PrevIncomplete, peer=Peer}) ->
    {Msgs, Incomplete} = split_by_crlf(PrevIncomplete ++ Data),
    Parsings = [cerlicue_parser:parse(M) || M <- Msgs],
    lists:foreach(fun(P) ->
                          forward_irc_msg(P, Peer)
                  end,
                  Parsings),
    NewState = State#s{incomplete_msg=Incomplete},
    {noreply, NewState};

handle_info({tcp_closed, _Sock}, State) ->
    {stop, tcp_closed, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal IRC callbacks
%% ------------------------------------------------------------------

forward_irc_msg({_P, "NICK", [Nick], _T}, Pid) ->
    cerlicue_irc_consumer:nick(Pid, Nick);
forward_irc_msg({_P, "USER", [Nick, Mode, _Unused], RealName}, Pid) ->
    cerlicue_irc_consumer:user(Pid, Nick, Mode, RealName);
forward_irc_msg({_P, "PRIVMSG", [Nick], Msg}, Pid) ->
    cerlicue_irc_consumer:privmsg(Pid, Nick, Msg);
forward_irc_msg({_P, "JOIN", [Channel], _T}, Pid) ->
    cerlicue_irc_consumer:join(Pid, Channel);
forward_irc_msg({_P, "PART", [Channel], Msg}, Pid) ->
    cerlicue_irc_consumer:part(Pid, Channel, Msg);
forward_irc_msg(Msg={_P, "MODE", [_Channel], _T}, _Pid) ->
    io:format("TODO: ~p~n", [Msg]);
forward_irc_msg({_P, "TOPIC", [Channel], _T}, Pid) ->
    cerlicue_irc_consumer:topic(Pid, Channel);
forward_irc_msg({_P, "NAMES", [Channel], _T}, Pid) ->
    cerlicue_irc_consumer:names(Pid, Channel);
forward_irc_msg({_Prefix, "QUIT", _Args, _Trail}, Pid) ->
    cerlicue_irc_consumer:quit(Pid);
forward_irc_msg({_P, "PONG", _As, _T}, _Pid) ->
    ok;
forward_irc_msg(Msg={_P, "CAP", ["REQ"], _Req}, _Pid) ->
    io:format("TODO: ~p~n", [Msg]);
forward_irc_msg(IrcMsg, _Pid) ->
    io:format("WTF: ~p~n", [IrcMsg]).

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
