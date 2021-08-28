-module(apns_event_callback).

-behaviour(apns_handler).

-export([handle_apns_error/2, 
		handle_apns_delete_subscription/1]).

handle_apns_error(MsgId, Status) ->
	lager:error("APNS: error: ~p- ~p ~n", [MsgId, Status]),
	ok.

handle_apns_delete_subscription(Data) ->
	lager:info("APNS: delete subscription: ~p~n", [Data]),
	ok.
