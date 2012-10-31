-module(cerlicue_irc_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, start_consumer/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSock) ->
    supervisor:start_link(?MODULE, LSock).

start_consumer(Sup, Sock) ->
    IrcConsumer = {cerlicue_irc_consumer,
                   {cerlicue_irc_consumer, start_link, [Sock]},
                   transient, 5000, worker, [cerlicue_irc_consumer]},
    supervisor:start_child(Sup, IrcConsumer).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(LSock) ->
    MaxRestarts = 0,
    InMaxSeconds = 1,
    IrcProducer = {cerlicue_irc_producer,
                   {cerlicue_irc_producer, start_link, [self(), LSock]},
                   transient, 5000, worker, [cerlicue_irc_producer]},
    {ok, { {one_for_all, MaxRestarts, InMaxSeconds}, [IrcProducer]} }.
