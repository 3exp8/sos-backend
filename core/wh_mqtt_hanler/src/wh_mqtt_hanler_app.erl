%%%-------------------------------------------------------------------
%% @doc wh_mqtt_hanler public API
%% @end
%%%-------------------------------------------------------------------

-module(wh_mqtt_hanler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wh_mqtt_hanler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
