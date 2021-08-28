-module(whistle_sup).

-export([start_link/0 ,init/1]).
-behaviour(supervisor).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).


init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,
     SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, []}}.
