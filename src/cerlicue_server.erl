-module(cerlicue_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(s, {nicks=dict:new(),
            channels=dict:new(),
            pids=dict:new()}).

-include_lib("proper/include/proper.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         nick/2,
         privmsg/3,
         join/2,
         mode/1,
         part/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Try to register a Nick to a Pid.
nick(Nick, Pid) ->
    gen_server:call(?SERVER, {nick, Nick, Pid}).

privmsg(Nick, Msg, Sender) ->
    gen_server:call(?SERVER, {privmsg, Nick, Msg, Sender}).

join(Channel, Client) ->
    gen_server:call(?SERVER, {join, Channel, Client}).

mode(_Channel) ->
    "+ns".

part(Channel, Client, Msg) ->
    gen_server:call(?SERVER, {part, Channel, Client, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #s{}}.

handle_call({nick, Nick, Pid},
            _From,
            State=#s{nicks=Nicks, pids=Pids}) ->
    case dict:find(Nick, Nicks) of
        {ok, _OtherPid} ->
            {reply, {error, 433}, State};
        error ->
            case dict:find(Pid, Pids) of
                {ok, _OldNick} ->
                    ok;
                error ->
                    erlang:monitor(process, Pid)
            end,
            NewNicks = dict:store(Nick, Pid, Nicks),
            NewPids = dict:store(Pid, Nick, Pids),
            {reply, ok, State#s{nicks=NewNicks, pids=NewPids}}
    end;

handle_call({privmsg, Channel="#"++_, Msg, Sender},
            _From,
            State=#s{channels=Channels, pids=Pids}) ->
    {ok, SenderNick} = dict:find(Sender, Pids),
    case dict:find(Channel, Channels) of
        {ok, Clients} ->
            Recipients = lists:delete(Sender, Clients),
            lists:foreach(fun(Recipient) ->
                                  Recipient ! {forward, Msg, SenderNick, Channel}
                          end,
                          Recipients),
            {reply, ok, State};
        error ->
            {reply, {error, 403}, State}
    end;

handle_call({privmsg, Nick, Msg, Sender},
            _From,
            State=#s{nicks=Nicks, pids=Pids}) ->
    {ok, SenderNick} = dict:find(Sender, Pids),
    case dict:find(Nick, Nicks) of
        {ok, Pid} ->
            Pid ! {privmsg, Msg, SenderNick},
            {reply, ok, State};
        error ->
            {reply, {error, 401}, State}
    end;

handle_call({join, Channel, Client},
            _From,
            State=#s{channels=Channels, pids=Pids}) ->
    Clients = case dict:find(Channel, Channels) of
        {ok, Cs} ->
            Cs;
        error ->
            []
    end,
    NewChannels = dict:store(Channel, [Client|Clients], Channels),
    Nicks = [dict:fetch(C, Pids) || C <- Clients],
    {reply, {ok, Nicks}, State#s{channels=NewChannels}};

handle_call({part, Channel, Client, Msg},
            _From,
            State=#s{channels=Channels, pids=Pids}) ->
    {ok, SenderNick} = dict:find(Client, Pids),
    case dict:find(Channel, Channels) of
        {ok, Clients} ->
            case lists:member(Client, Clients) of
                true ->
                    OtherClients = lists:delete(Client, Clients),
                    lists:foreach(fun(Recipient) ->
                                          Recipient ! {part, Msg, SenderNick, Channel}
                                  end,
                                  OtherClients),
                    NewChannels = dict:store(Channel, OtherClients, Channels),
                    {reply, ok, State#s{channels=NewChannels}};
                false ->
                    {reply, {error, 442}, State}
            end;
        error ->
            {reply, {error, 403}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason},
            State=#s{nicks=Nicks, pids=Pids, channels=Channels}) ->
    {ok, Nick} = dict:find(Pid, Pids),
    io:format("~p is down~n", [Nick]),
    NewNicks = dict:erase(Nick, Nicks),
    NewPids = dict:erase(Pid, Pids),
    NewChannels = remove_client(Pid, Channels),
    {noreply, State#s{nicks=NewNicks, channels=NewChannels, pids=NewPids}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

remove_client(Client, Channels) ->
    dict:fold(fun(ChannelName, Clients, Acc) ->
                      NewClients = lists:delete(Client, Clients),
                      dict:store(ChannelName, NewClients, Acc)
              end,
              dict:new(),
              Channels).
