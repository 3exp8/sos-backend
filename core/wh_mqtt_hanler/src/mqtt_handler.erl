-module(mqtt_handler).
-export([signal/4, signal/5, signal_rate/4]).

signal_rate(Topic, Action, Type, Payload) ->
	mqtt_publish(<<>>, Topic, Action, Type, Payload, 2).


signal(Topic, Action, Type, Payload) ->
	signal(<<>>, Topic, Action, Type, Payload).

signal(Source, Topic, Action, Type, Payload) ->
	mqtt_publish(Source, Topic, Action, Type, Payload, 2).

mqtt_publish(Source, Topic, Action, Type, Payload, QoS) ->
	lager:info("Source ~p; Topic ~p ~n", [Source, Topic]),
	PublishedJson = jsx:encode([{<<"type">>, Type},
															{<<"action">>, Action},
															{<<"source">>, Source},
															{<<"content">>, Payload}]),
	zt_mqtt:publish(Topic, PublishedJson, QoS).

