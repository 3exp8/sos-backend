-module(whistle_amqp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	_ = start_deps(),
	wh_amqp_sup:start_link().
    % whistle_amqp:start_link(). 

stop(_State) ->
    ok.

start_deps() ->
    _ = [wh_util:ensure_started(App) || App <- [sasl, amqp_client]],
    ok.