-module(crossbar_handler_pool).

-export([init/1,
		start_link/0]).

-behaviour(supervisor).

-include("crossbar.hrl").

-define(CHILDREN, [
                  ?SUPER('hook_handle_pool')
                  ]
       ).



-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).	

init([]) ->
	
	RestartStrategy = 'one_for_one',
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
