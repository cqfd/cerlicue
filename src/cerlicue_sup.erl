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
    ServerSpec = {cerlicue_server, {cerlicue_server, start_link, []},
                  permanent, 5000, worker, [cerlicue_server]},
    TcpSupSpec = {cerlicue_tcp_sup, {cerlicue_tcp_sup, start_link, []},
                  permanent, infinity, supervisor, [cerlicue_tcp_sup]},
    {ok, { {one_for_all, MaxRestarts, InMaxSeconds}, [ServerSpec, TcpSupSpec]} }.
