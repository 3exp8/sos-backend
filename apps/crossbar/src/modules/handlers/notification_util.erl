-module(notification_util).

-include("crossbar.hrl").

-export([
			create_push_content/1,
			create_and_push/1,
			create_mqtt/1,
			push/4
		]).

create_and_push(Data) -> 
	Uuid = zt_util:get_uuid(),
    BaseInfo = #{
		id => <<"notification", Uuid/binary>>,
		created_time => zt_util:now_to_utc_binary(os:timestamp())
	},
	NotificationInfo = maps:merge(BaseInfo,Data),
	notification_db:save(NotificationInfo),
	PushContent = fcm_create_push_content(NotificationInfo),
	Destinations = maps:get(destinations,Data,[]),
	push_to_destinations(Destinations,PushContent).

create_mqtt(Data) -> 
Uuid = zt_util:get_uuid(),
BaseInfo = #{
	id => <<"notification", Uuid/binary>>,
	created_time => zt_util:now_to_utc_binary(os:timestamp())
},
NotificationInfo = maps:merge(BaseInfo,Data),
notification_db:save(NotificationInfo),
#{
	target_type := TransType,
	notification := NotificationData
} = Data,
Destinations = maps:get(destinations,Data,[]),
lists:foreach(fun(#{
	type := Type,
	id := CustomerId
}) -> 
	if 
		Type == <<"customer">> -> 
			Topic = <<>>,
			Payload = #{
				type => TransType,
				data => NotificationData
			},
			Props = maps:to_list(Payload),
  			PublishedJson = jsx:encode(Props),
			zt_mqtt:publish(Topic,PublishedJson);
		true -> ok
	end
end, Destinations).

push_to_destinations([],_) -> ok;

push_to_destinations([DestinationInfo|OtherDestinations], PushContent) -> 
	#{
		type := Type,
		id := Id
	} = DestinationInfo,
	Devices = find_destination_by_type(Type, Id),
	lists:foreach(fun(#{ id:= DeviceId, push_id := PushId})-> 
		lager:debug("DeviceId: ~p, PushId: ~p, Push Content: ~p~n",[DeviceId, PushId, PushContent]),
		CurrentId = maps:get(id,PushContent,<<>>),
		NewPushContent = maps:merge(PushContent, #{
			id => <<CurrentId/binary,"_",DeviceId/binary>>
		}),
		lager:debug("NewPushContent: ~p~n",[NewPushContent]),
		zt_push:push(PushId,NewPushContent)
	end,Devices),
	push_to_destinations(OtherDestinations, PushContent).

find_destination_by_type(<<"account">>, Id) -> 
	device_db:find_active_by_account_id(Id);

find_destination_by_type(<<"user">>, Id) -> 
	device_db:find_active_by_user_id(Id);

find_destination_by_type(<<"customer">>, Id) -> 
	device_db:find_active_by_customer_id(Id);
 
find_destination_by_type(_, _Id) -> [].

push(Destinations, NoInfo, DataInfo, Opts) ->
	Uuid = zt_util:get_uuid(),
	Id = <<"notification", Uuid/binary>>,
	Data = get_data_info(DataInfo),
	TargetType = maps:get(target_type, Opts, <<>>),
	TargetId = maps:get(target_id, Opts, <<>>),
	Description = maps:get(description, Opts, <<>>),
	CreatedBy = maps:get(created_by, Opts, <<>>),

	NotificationDb =  
			#{
				id => Id,
				destinations => Destinations,
				target_type => TargetType,
				target_id => TargetId,
				type => <<>>,
				notification => get_notification_info(NoInfo),
				data => Data,
				description => Description,
				created_by => CreatedBy,
				created_time => zt_util:now_to_utc_binary(os:timestamp())
			},
	notification_db:save(NotificationDb),
	PushContent = create_push_content(NotificationDb),
	push_handler:push_destinations(Destinations, PushContent).

fcm_create_push_content(NotificationInfo) -> 
	TargetType = maps:get(target_type, NotificationInfo, <<>>),
	TargetId = maps:get(target_id, NotificationInfo, <<>>),
	BaseInfo = #{
		type => TargetType,
		id => TargetId
	},
	WithNotificationInfo = fcm_create_notification_push_content(BaseInfo, NotificationInfo),
	fcm_create_notification_push_content(WithNotificationInfo,NotificationInfo).

fcm_create_notification_push_content(PushData, #{
	notification := #{
		title := Title,
		body := Body
	}
}) -> 
   maps:merge(PushData, #{ 
	   notification => #{
	   title => Title,
	   body => Body
   }}
);

fcm_create_notification_push_content(PushData,_) -> PushData.

fcm_create_data_push_content(PushData,
		#{
			data := Data
		}
		) when is_map(Data) -> 

	maps:merge(PushData, #{ 
			data => Data
		}
	);

fcm_create_data_push_content(PushData,_) -> PushData.



create_push_content(#{
					id := Id,
					data := DataInfo,
					type := <<"data">>
				}) ->
	#{
		id => Id,
		data => DataInfo
	 };

create_push_content(#{
			id := Id,
			notification := NoInfo,
			type := <<"notification">>
		}) ->
	#{
		id => Id,
		notification => NoInfo
	 };

create_push_content(#{
		id := Id,
		notification := NoInfo,
		data := DataInfo
	}) ->
	#{
		id => Id,
		notification => NoInfo,
		data => DataInfo
	}.

get_notification_info(NoInfo) when is_map(NoInfo) ->
	lager:info("send_push: NoInfo: ~p ~n", [NoInfo]),
	NoInfo;

get_notification_info(_NoInfo)-> #{}.

get_data_info(DataInfo) when is_map(DataInfo) -> DataInfo;

get_data_info(_DataInfo) -> #{}.