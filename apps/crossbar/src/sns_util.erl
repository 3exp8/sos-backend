-module(sns_util).
-export([
		push_callback/2,
		push_callback/3
	]).

push_callback(Status, ErrorData, ResultData) ->
	lager:debug("push_callback callack: Status: ~p, ErrorData: ~p, ResultData: ~p~n",[Status, ErrorData, ResultData]).

push_callback(#{
			id := Id			
		} = MetaData, ResultData) ->
	lager:debug("Send push successfully: MetaData: ~p, ResultData: ~p~n",[MetaData, ResultData]),
	case notification_db:find(Id) of 
		notfound -> 
			lager:warning("Notification id ~p notfound~n",[Id]),
			ok;
		#{
			detail_results := CurResults
		} = NotificationInfo ->
			ResultData2 = maps:merge(ResultData, maps:without([id], MetaData)),
			save_push_callback(NotificationInfo, CurResults,ResultData2)
	end;

push_callback(_MetaData, _ResultData) ->
	lager:debug("push_callback other MetaData: ~p, ResultData: ~p~n",[_MetaData,_ResultData]).

save_push_callback(NotificationInfo, CurResults, CbResultData) ->

	NewNotificationInfo = maps:merge(NotificationInfo, #{
		detail_results => [CbResultData|CurResults]
	}),
	notification_db:save(NewNotificationInfo).