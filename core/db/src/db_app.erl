-module(db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	lager:info("db_app start with 2 params.",[]),
    db_sup:start_link().

stop(_State) ->
    ok.
