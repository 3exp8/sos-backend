-module(cb_wh_mqtt).
-export([subscribe/0, receive_mqtt_msg/2]).

subscribe() ->
	Options = application:get_env(wh_ex_mqtt, mqtt, []),
	Topics = proplists:get_value(topics, Options, 100),
	Module = proplists:get_value(module, Options),
	QOS = proplists:get_value(qos, Options, 1),
	wh_ex_mqtt:subscribe(Module, Topics, QOS),
	zt_mqtt:subscribe(Module, <<"xe/currency_rate">>, QOS).

receive_mqtt_msg(Topic, Payload) ->
	lager:info("receive_mqtt_msg Topic ~p ~n", [Topic]),
	%Type = proplists:get_value(<<"type">>, Payload, <<>>),
	Content = proplists:get_value(<<"content">>, Payload, <<>>).
