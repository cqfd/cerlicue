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
    BackendSpec = {cerlicue_backend,
                   {cerlicue_backend, start_link, []},
                   permanent, 5000, worker, [cerlicue_backend]},
    IrcSupSpec = {cerlicue_irc_sup,
                  {cerlicue_irc_sup, start_link, []},
                  permanent, infinity, supervisor, [cerlicue_irc_sup]},
    {ok, { {one_for_all, MaxRestarts, InMaxSeconds}, [BackendSpec, IrcSupSpec]} }.
