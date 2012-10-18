-module(cerlicue_tcp_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, LSock} = gen_tcp:listen(6667, [{reuseaddr, true}]),
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [LSock]),
    start_child(),
    {ok, Pid}.

start_child() ->
    supervisor:start_child(?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSock]) ->
    MaxRestarts = 0,
    InMaxSeconds = 1,
    RestartStrategy = {simple_one_for_one, MaxRestarts, InMaxSeconds},
    TcpHandlerSpec = {cerlicue_tcp,
                      {cerlicue_tcp, start_link, [LSock]},
                      temporary, brutal_kill, worker,
                      [cerlicue_tcp]},
    {ok, {RestartStrategy, [TcpHandlerSpec]}}.
