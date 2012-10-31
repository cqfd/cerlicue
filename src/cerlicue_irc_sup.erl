-module(cerlicue_irc_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, LSock} = gen_tcp:listen(6667, [{reuseaddr, true}]),
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, LSock),
    start_child(),
    {ok, Sup}.

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(LSock) ->
    MaxRestarts = 0,
    InMaxSeconds = 1,
    IrcServerSupSpec = {cerlicue_irc_server_sup,
                        {cerlicue_irc_server_sup, start_link, [LSock]},
                        temporary, 5000, worker, [cerlicue_irc_server_sup]},
    {ok, { {simple_one_for_one, MaxRestarts, InMaxSeconds}, [IrcServerSupSpec]} }.
