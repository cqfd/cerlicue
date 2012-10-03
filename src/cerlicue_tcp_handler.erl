-module(cerlicue_tcp_handler).
-behaviour(gen_server).

-define(PING_TIMEOUT, 10 * 1000).
-define(PING_INTERVAL, 10 * 1000).

-record(s, {sock, incomplete_msg="", idle=false}).

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
    cerlicue_tcp_handler_sup:start_child(),
    {noreply, #s{sock=Sock, idle=false}, ?PING_INTERVAL};

handle_info(timeout, State=#s{idle=false}) ->
    send_ping(State#s.sock),
    {noreply, State#s{idle=true}, ?PING_TIMEOUT};

handle_info(timeout, State=#s{idle=true}) ->
    {stop, ping_timeout, State};

handle_info({tcp, _Sock, Data}, State=#s{incomplete_msg=IncompleteMsg}) ->
    case split_by_crlf(Data) of
        {[FirstMsg|RestMsgs], Partial} ->
            CompletedMsg = IncompleteMsg ++ FirstMsg,
            Parsings = lists:map(fun cerlicue_parser:parse/1, [CompletedMsg|RestMsgs]),
            lists:foreach(make_msg_handler(State), Parsings),
            NewState = State#s{incomplete_msg=Partial};
        {[], Partial} ->
            NewState = State#s{incomplete_msg=IncompleteMsg ++ Partial, idle=false}
    end,
    {noreply, NewState, ?PING_INTERVAL};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal IRC callbacks
%% ------------------------------------------------------------------

make_msg_handler(#s{sock=Sock}) ->
    fun (Parsing) ->
            Response = io_lib:format("~p~c~c", [Parsing, $\r, $\n]),
            gen_tcp:send(Sock, Response)
    end.

send_ping(Sock) ->
    Msg = io_lib:format("PING~c~c", [$\r, $\n]),
    gen_tcp:send(Sock, Msg).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% CrlfSeparatedMsgs -> {[Msgs], PartialMsg}
split_by_crlf(CrlfSeparatedMsgs) ->
    split_by_crlf(CrlfSeparatedMsgs, "", []).

split_by_crlf("", ReversedPartial, Accumulated) ->
    Partial = lists:reverse(ReversedPartial),
    {lists:reverse(Accumulated), Partial};
split_by_crlf("\r\n" ++ Msg, ReversedPartial, Accumulated) ->
    Complete = lists:reverse(ReversedPartial),
    split_by_crlf(Msg, "", [Complete|Accumulated]);
split_by_crlf([C|Rest], ReversedPartial, Accumulated) ->
    split_by_crlf(Rest, [C|ReversedPartial], Accumulated).
