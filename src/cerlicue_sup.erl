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
    RestartStrategy = {one_for_one, MaxRestarts, InMaxSeconds},
    TcpSupSpec = {cerlicue_tcp_handler_sup,
                  {cerlicue_tcp_handler_sup, start_link, []},
                  permanent, 5000, supervisor,
                  [cerlicue_tcp_handler_sup]},
    {ok, {RestartStrategy, [TcpSupSpec]}}.
