-module(cb_notifications).

-include("crossbar.hrl").

-define(PATH_READ, <<"read">>).

-export([init/0
				 ,validate/1
				 ,validate/2
				 ,validate/3
				 ,resource_exists/0
				 ,resource_exists/1
				 ,resource_exists/2
				 ,authenticate/1
				 ,authenticate/2
				 ,authenticate/3
				 ,authorize/1
				 ,authorize/2
				 ,authorize/3
				 ,allowed_methods/0
				 ,allowed_methods/1
				 ,allowed_methods/2
				 ,handle_get/1
				 ,handle_get/2
				 ,handle_put/1
				 ,handle_post/3
				 ,handle_delete/2
				]).

-export([
          permissions/0
  ]).


init() ->
	_ = crossbar_bindings:bind(<<"*.resource_exists.notifications">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.notifications">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.notifications">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.authorize.notifications">>, ?MODULE, 'authorize'),
	_ = crossbar_bindings:bind(<<"*.allowed_methods.notifications">>, ?MODULE, 'allowed_methods'),
	_ = crossbar_bindings:bind(<<"*.to_json.get.notifications">>, ?MODULE, 'handle_get'),
	_ = crossbar_bindings:bind(<<"*.execute.post.notifications">>, ?MODULE, 'handle_post'),
	_ = crossbar_bindings:bind(<<"*.execute.put.notifications">>, ?MODULE, 'handle_put'),
	_ = crossbar_bindings:bind(<<"*.execute.delete.notifications">>, ?MODULE, 'handle_delete').

allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) ->
	[?HTTP_GET, ?HTTP_DELETE].

allowed_methods(_Id, _Path) ->
	[?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_Id) ->'true'.
resource_exists(_Id, ?PATH_READ) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(_Context) ->
	Token = cb_context:auth_token(_Context),
	app_util:oauth2_authentic(Token, _Context).

authenticate(Context, _Path) ->
	Token = cb_context:auth_token(Context),
	app_util:oauth2_authentic(Token, Context).

authenticate(Context, _Id, ?PATH_READ) ->
	Token = cb_context:auth_token(Context),
	app_util:oauth2_authentic(Token, Context);

authenticate(_Context, _Id, _Path) ->
	false.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(_Context) -> true.

authorize(_Context, _Id) -> true.

authorize(_Context, _Id, _Path) -> true.

-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

validate(Context, Id) ->
	validate_request(Id, Context, cb_context:req_verb(Context)).

validate(Context, Id, Path) ->
	validate_request(Id, Context, Path, cb_context:req_verb(Context)).

%% Handle GET Method
-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}) ->
	QueryJson = cb_context:query_string(Context),
	Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
	Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	Role = cb_context:role(Context),
	lager:debug("Role = ~p~n", [Role]),
	case Role of
		?USER_ROLE_CUSTOMER ->
			Customer_id = cb_context:user_id(Context),
			Notifications = notification_db:find_by_conditions([{"destinations#id", Customer_id}], PropQueryJson, Limit, Offset);
		?USER_ROLE_USER ->
			User_id = cb_context:user_id(Context),
			Notifications = notification_db:find_by_conditions([{"destinations#id", User_id}], PropQueryJson, Limit, Offset)
	end,
	PropNotifications = lists:map(
			fun(Notification) -> get_sub_fields_notification(Notification) 
	end, Notifications),
	{Req, cb_context:setters(Context
					,[{fun cb_context:set_resp_data/2, PropNotifications}
					  ,{fun cb_context:set_resp_status/2, 'success'}
					])}.

handle_get({Req, Context}, Id) ->
	case notification_db:find(Id) of
		#{id := Id} = NotificationInfo ->
			PropNotification = get_sub_fields_notification(NotificationInfo),
			{Req, cb_context:setters(Context
				,[{fun cb_context:set_resp_data/2, PropNotification}
				,{fun cb_context:set_resp_status/2, 'success'}
			])};
		_ ->
			{Req, cb_context:setters(Context, [
				{fun cb_context:set_resp_error_msg/2, <<"Notification not found.">>},
				{fun cb_context:set_resp_status/2, <<"error">>},
				{fun cb_context:set_resp_error_code/2, 404}
			])}
	end.

%% Handle PUT Method
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
	ReqJson = cb_context:req_json(Context),
	Destinations = wh_json:get_value(<<"destinations">>, ReqJson, []),
	TargetType = wh_json:get_value(<<"target_type">>, ReqJson, <<>>),
	TargetId = wh_json:get_value(<<"target_id">>, ReqJson, <<>>),
	Icon = wh_json:get_value(<<"icon">>, ReqJson, <<>>),
	Content = wh_json:get_value(<<"content">>, ReqJson, <<>>),
	ContentType = wh_json:get_value(<<"content_type">>, ReqJson, <<>>),
	NotificationMsg = wh_json:get_value(<<"notification_message">>, ReqJson, Content),
	Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
	Title = wh_json:get_value(<<"title">>, ReqJson, <<>>),
	Image = wh_json:get_value(<<"image">>, ReqJson, <<>>),
	Description = wh_json:get_value(<<"description">>, ReqJson, <<>>),
	Role = cb_context:role(Context),
	CreatedTime = zt_util:now_to_utc_binary(os:timestamp()),
	CreatedBy = case Role of
		?USER_ROLE_CUSTOMER ->
			cb_context:customer_id(Context);
		?USER_ROLE_USER ->
			cb_context:user_id(Context)
	end,
	Uuid = zt_util:get_uuid(),
	Id = <<"notification", Uuid/binary>>,
	NotificationDb =  #{
		id => Id,
		destinations => Destinations,
		target_type => TargetType,
		target_id => TargetId,
		type => Type,
		notification => #{
			title => Title,
			body => NotificationMsg
		},
		data => #{
			content => Content,
			content_type => ContentType
		},
		description => Description,
		created_by => CreatedBy,
		created_time => CreatedTime
	},
	notification_db:save(NotificationDb),
	PushContent = notification_util:create_push_content(NotificationDb),
	notification_util:push_destinations(Destinations, PushContent),
	%notification_util:send(NotificationDb),
	cb_context:setters(Context,
						[{fun cb_context:set_resp_data/2, NotificationDb},
						{fun cb_context:set_resp_status/2, 'success'}]).

%% Handle READ Method
-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, Id, ?PATH_READ) ->
	Role = cb_context:role(Context),
	Time = zt_util:now_to_utc_binary(os:timestamp()),
	{Type, UserId, Fullname} = get_read_by_info(Role, Context),
	case notification_db:find_by_read_by(Type, UserId, Id) of
		[] ->
			ReadByInfo = [#{
				name => Fullname,
				time => Time,
				id => UserId,
				type => Type
			}],
			NotificationInfo = notification_db:find(Id),
			lager:debug("NotificationInfo = ~p~n", [NotificationInfo]),
			ReadByList = notification_doc:read_by(NotificationInfo),
			NewReadByList = lists:append(ReadByList, ReadByInfo),
			% lager:debug("NewReadByList: ~p~n", [NewReadByList]),
			NewReadByInfo = 
				maps:merge(NotificationInfo, #{
					read_by => NewReadByList,
					updated_by => UserId,
					updated_time => zt_util:now_to_utc_binary(os:timestamp())
				}),
			notification_db:save(NewReadByInfo),
			RespData = get_sub_fields_notification(NewReadByInfo),
			cb_context:setters(Context,[
					{fun cb_context:set_resp_data/2, RespData}
					,{fun cb_context:set_resp_status/2, 'success'}
				]);
		_ ->
			cb_context:setters(Context,[
							{fun cb_context:set_resp_data/2, <<"read">>}
						  ,{fun cb_context:set_resp_status/2, 'success'}
						])
	end.

get_read_by_info(?USER_ROLE_CUSTOMER, Context) ->
	CustomerId = cb_context:customer_id(Context),
	Fullname = 
	case customer_db:find(CustomerId) of 
		#{first_name := Firstname, last_name := Lastname} -> 
			<<Firstname/binary, " ", Lastname/binary>>;
		_ -> <<>>
	end,
	{<<"customer">>, CustomerId, Fullname};

get_read_by_info(?USER_ROLE_USER, Context) ->
	UserId = cb_context:user_id(Context),
	Fullname = 
	case user_db:find(UserId) of 
		#{first_name := Firstname, last_name := Lastname} -> 
			<<Firstname/binary, " ", Lastname/binary>>;
		_ -> <<>>
	end,
	{<<"user">>, UserId, Fullname}.

%% Handle Delete Method
-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
	RespData = notification_db:del_by_id(Id),
	lager:info("delete ", RespData),
	cb_context:setters(Context, [
		{fun cb_context:set_resp_data/2, RespData}
	   ,{fun cb_context:set_resp_status/2, 'success'}
	]).

permissions() ->
  authorize_util:default_permission(?MODULE).
  
validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?HTTP_PUT) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
					,[{fun cb_context:set_resp_status/2, 'success'}]),
	ValidateFuns = [
			fun validate_destinations/2,
			fun validate_target_type/2,
		  % fun validate_target_id/2,
			fun validate_content/2,
			fun validate_content_type/2
	 ],
	lists:foldl(fun(F, C) ->
		F(ReqJson, C)
	end, Context1, ValidateFuns);

validate_request(Context, _Verb) ->
	Context.

validate_request(_Id, Context, ?HTTP_GET) ->
	cb_context:setters(Context
						,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
	cb_context:setters(Context
					 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, _Verb) ->
	Context.

validate_request(_Id, Context, ?PATH_READ, ?HTTP_POST) ->
	cb_context:setters(Context,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, _Path, _Verb) ->
	Context.

-spec validate_destinations(api_binary(), cb_context:context()) -> cb_context:context().
validate_destinations(ReqJson, Context) ->
	Destinations = wh_json:get_value(<<"destinations">>, ReqJson, []),
	case Destinations of
		[] ->
			api_util:validate_error(Context, <<"destinations">>, <<"required">>, <<"Field 'destinations' is required">>);
		_ ->
			Context
	end.

-spec validate_target_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_target_type(ReqJson, Context) ->
	TargetType = wh_json:get_value(<<"target_type">>, ReqJson, <<>>),
	case TargetType of
		<<>> ->
			api_util:validate_error(Context, <<"target_type">>, <<"required">>, <<"Field 'target_type' is required">>);
		_ ->
			Context
	end.

-spec validate_content(api_binary(), cb_context:context()) -> cb_context:context().
validate_content(ReqJson, Context) ->
	Content = wh_json:get_value(<<"content">>, ReqJson, <<>>),
	case Content of
		<<>> ->
			api_util:validate_error(Context, <<"content">>, <<"required">>, <<"Field 'content' is required">>);
		_ ->
			Context
	end.

-spec validate_content_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_content_type(ReqJson, Context) ->
	ContentType = wh_json:get_value(<<"content_type">>, ReqJson, <<>>),
	case ContentType of
		<<>> ->
			api_util:validate_error(Context, <<"content_type">>, <<"required">>, <<"Field 'content_type' is required">>);
		_ ->
			Context
	end.

get_sub_fields_notification(Notification) ->
	Fields = [created_by, created_time],
	NewMap = maps:without(Fields, Notification),
	Res = maps:to_list(NewMap),
	proplists:substitute_aliases([], Res).