-module(cerlicue_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    MaxRestarts = 0,
    InMaxSeconds = 1,
    RestartStrategy = {one_for_all, MaxRestarts, InMaxSeconds},
    RouterSpec = {cerlicue_router, {cerlicue_router, start_link, []},
                  permanent, 5000, worker, [cerlicue_router]},
    ChannelSupSpec = {cerlicue_channel_sup, {cerlicue_channel_sup, start_link, []},
                      permanent, infinity, supervisor,
                      [cerlicue_channel_sup]},
    {ok, {RestartStrategy, [RouterSpec, ChannelSupSpec]}}.
