-module(cerlicue_channel_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Nick) ->
    supervisor:start_child(?SERVER, [Nick]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    MaxRestarts = 0,
    InMaxSeconds = 1,
    RestartStrategy = {simple_one_for_one, MaxRestarts, InMaxSeconds},
    ChannelSpec = {cerlicue_channel,
                   {cerlicue_channel, start_link, []},
                   temporary, brutal_kill, worker,
                   [cerlicue_channel]},
    {ok, {RestartStrategy, [ChannelSpec]}}.
