-module(push_handler).
-include("crossbar.hrl").
-include("push.hrl").
-export([push_destinations/2]).

get_notification(Title, NoInfo) ->
	Body = maps:get(body, NoInfo, <<>>),
	ContentType = maps:get(content_type, NoInfo, <<>>),
	#{
		title => Title,
		body => Body, 
		content_type => ContentType
	}.

get_payload(NoInfo, Doc) ->
	WithoutFields = maps:without([notification], Doc),
	maps:merge(#{notification => NoInfo}, WithoutFields).

get_query({FieldName, Ids}, Opts) when is_list(Ids) ->
	[{FieldName, 'in', Ids }| Opts];

get_query({FieldName, Id}, Opts) when is_binary(Id) ->
	[{FieldName, Id} | Opts].

push_destinations([#{id := Id, type := Type}|Destinations], Body) ->
	push_with_des_type(Type, Id, Body),
	push_destinations(Destinations, Body);

push_destinations([], _) -> ok.

push_with_des_type(?PUSH_DES_TYPE_CUSTOMER, AccountId, #{notification := NoInfo} = Doc) ->
	lager:info("send_push: ~p ~n", [?PUSH_DES_TYPE_CUSTOMER]),
	Title = maps:get(customer_title, NoInfo, <<>>),
	Notification = get_notification(Title, NoInfo),
	Payload = get_payload(Notification, Doc),
	Query = get_query({account_id, AccountId}, [{account_scope, ?USER_ROLE_CUSTOMER}]),
	push_with_condition(Query, Payload);

push_with_des_type(?PUSH_DES_TYPE_USER, AccountId, #{notification := NoInfo} = Doc) ->
	lager:info("send_push: ~p ~n", [?PUSH_DES_TYPE_USER]),
	Title = maps:get(user_title, NoInfo, <<>>),
	Notification = get_notification(Title, NoInfo),
	Payload = get_payload(Notification, Doc),
	Query = get_query({account_id, AccountId}, [{account_scope, ?USER_ROLE_USER}]),
	push_with_condition(Query, Payload);

push_with_des_type(?PUSH_DES_TYPE_ACCOUNT, AccountId, Body) ->
	lager:info("send_push: ~p ~n", [?PUSH_DES_TYPE_ACCOUNT]),
	case user_db:find_by_account_id(AccountId) of
		[] ->
			lager:info("send_push: notfound user");
		Users ->
			UserIds = lists:map(fun(#{id := Id}) -> Id end, Users),
			lager:info("send_push: UserIds ~p ~n", [UserIds]),
			push_with_des_type(?PUSH_DES_TYPE_USER, UserIds, Body)
	end.

push_with_condition(Query, Body) when is_list(Query) ->
	case device_db:find_to_push(Query) of
		[] ->
			lager:info("send_push devices empty ~p ~n", [Query]);
		Devices ->
			lager:info("send_push Devices ~p ~n", [Devices]),
			push_with_devices(zt_util:to_map(Body), Devices)
	end.

push_with_devices(Data, Devices) ->
	Notification = maps:get(notification, Data, <<>>),
	lists:foreach(fun(V) ->
		PushId = maps:get(push_id, V, <<>>),
		Payload = #{
						to => PushId,
						priority => ?PRIORITIZE,
						time_to_live => ?TTL,
						notification => Notification,
						data => Data
		},
		zt_push:push(PushId, Payload)
    end,  Devices).