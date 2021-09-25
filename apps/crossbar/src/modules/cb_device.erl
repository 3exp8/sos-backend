%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API test demo
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_device).

-include("crossbar.hrl").

-export([
          init/0
         ,validate/1
         ,validate/2
         ,resource_exists/0
         ,resource_exists/1
         ,authenticate/1
         ,authenticate/2
         ,authorize/1
         ,authorize/2
         ,allowed_methods/0
         ,allowed_methods/1
         ,handle_get/1
         ,handle_get/2
         ,handle_post/2
         ,handle_put/1
         ,update_device/3
         ,add_device/2
         ,update_status/2
         ,add_device/6
         ,add_client/3
        ]).

-export([
         permissions/0
        ]).


init() ->
  _ = crossbar_bindings:bind(<<"*.resource_exists.devices">>, ?MODULE, 'resource_exists'),
  _ = crossbar_bindings:bind(<<"*.validate.devices">>, ?MODULE, 'validate'),
  _ = crossbar_bindings:bind(<<"*.authenticate.devices">>, ?MODULE, 'authenticate'),
  _ = crossbar_bindings:bind(<<"*.authorize.devices">>, ?MODULE, 'authorize'),
  _ = crossbar_bindings:bind(<<"*.allowed_methods.devices">>, ?MODULE, 'allowed_methods'),
  _ = crossbar_bindings:bind(<<"*.content_types_provided.devices">>, ?MODULE, 'content_types_provided'),
  _ = crossbar_bindings:bind(<<"*.content_types_accepted.devices">>, ?MODULE, 'content_types_accepted'),
  _ = crossbar_bindings:bind(<<"*.execute.put.devices">>, ?MODULE, 'handle_put'),
  _ = crossbar_bindings:bind(<<"*.execute.post.devices">>, ?MODULE, 'handle_post'),
  _ = crossbar_bindings:bind(<<"*.to_json.get.devices">>, ?MODULE, 'handle_get').


%% /api/v1/test
allowed_methods() -> [?HTTP_GET,?HTTP_PUT].

%% /api/v1/test/{id}
allowed_methods(_Id) -> [?HTTP_GET, ?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.

%% /api/v1/test
resource_exists() -> 'true'.

%% /api/v1/test/{id}
resource_exists(_Id) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token,Context).

authenticate(Context, _Id) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token,Context).

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context) -> true.

authorize(_Context, _Id) -> true.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/test
validate(Context) ->
  validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/test/{id}
validate(Context, Id) ->
  validate_request(Id, Context, cb_context:req_verb(Context)).


handle_put(Context) ->
  ReqJson = cb_context:req_json(Context),
  DeviceId =
    case wh_json:get_value(<<"device_id">>, ReqJson, <<>>) of 
        <<>> ->  
          Uuid = zt_util:get_uuid(),
          <<"device",Uuid/binary>>;
        ReqDeviceId -> ReqDeviceId
    end,

  Role = cb_context:role(Context),
  UserInfo = 
    case Role of 
      ?USER_ROLE_USER -> 
          #{
            account_id => cb_context:account_id(Context),
            client_id => cb_context:user_id(Context)
          };
       _ -> 
        #{
          client_id => cb_context:customer_id(Context)
        }
    end,

  DeviceDb = 
    case device_db:find(DeviceId) of 
      notfound -> #{};
      TempDeviceDb -> TempDeviceDb
    end,

  CreatedTime = zt_datetime:get_now(),
  AppId = wh_json:get_value(<<"app_id">>, ReqJson, <<>>),
  PushId = wh_json:get_value(<<"push_id">>, ReqJson, maps:get(push_id,DeviceDb, <<>>)),
  OsType = wh_json:get_value(<<"os_type">>, ReqJson, maps:get(os_type,DeviceDb, <<>>)),
  Info = 
    maps:merge(
      #{
            id => DeviceId,
            account_scope => Role,
            created_time => CreatedTime,
            updated_time => CreatedTime,
            app_id => AppId,
            os_type => OsType,
            push_id => PushId,
            status => <<"active">>
          }, UserInfo),
  device_db:save(Info),
  lager:info("push_device: created device ~p ~n", [Info]),  
  ResponseData =  [{<<"device_id">>,  DeviceId}],
  cb_context:setters(Context
                               ,[{fun cb_context:set_resp_data/2, ResponseData}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).

handle_get({Req, Context}) ->
  QueryJson = cb_context:query_string(Context),
  Role = cb_context:role(Context),
  CustomerId = cb_context:customer_id(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  if Role == ?USER_ROLE_ADMIN;
      Role == ?USER_ROLE_CUSTOMER ->
       DeviceList = get_devices(Role, CustomerId, QueryJson, Limit, Offset),
       FilteredDevices = lists:map(fun(DeviceInfo) -> get_sub_fields_device(Role, DeviceInfo) end, DeviceList),
       {Req, cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, FilteredDevices}
                                          ,{fun cb_context:set_resp_status/2, 'success'}])};
     true ->
       {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                          {fun cb_context:set_resp_status/2, <<"error">>},
                                          {fun cb_context:set_resp_error_code/2, 403}])}

  end.

handle_get({Req, Context},DeviceId) ->
  Role = cb_context:role(Context),
  CustomerId = cb_context:customer_id(Context),
  case  device_db:find(DeviceId) of
    #{customer_id := CustomerIdDb} = DeviceInfo ->
      if Role == ?USER_ROLE_ADMIN;
         Role == ?USER_ROLE_CUSTOMER andalso CustomerId == CustomerIdDb ->
           FilteredInfo = get_sub_fields_device(Role, DeviceInfo),
           lager:info("FilteredInfo ~p ~n", [FilteredInfo]),
           {Req, cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, FilteredInfo}
                                              ,{fun cb_context:set_resp_status/2, 'success'}])};

         true -> {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                                    {fun cb_context:set_resp_status/2, <<"error">>},
                                                    {fun cb_context:set_resp_error_code/2, 403}])}
         end;
    _ ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"Devices Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}
                         ])
  end.

handle_post(Context, DeviceId) ->
  CustomerId = cb_context:customer_id(Context),
  Role = cb_context:role(Context),
  ReqJson = cb_context:req_json(Context),
  case device_db:find(DeviceId) of
    #{customer_id := CustomerIdDb} = Device ->
      if  Role == ?USER_ROLE_ADMIN;
        Role == ?USER_ROLE_CUSTOMER andalso CustomerId == CustomerIdDb ->
            DeviceInfo = update_device(ReqJson, Device, CustomerId),
            device_db:save(DeviceInfo),
            RespData = get_sub_fields_device(Role, DeviceInfo),
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_data/2, RespData}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]);
          true ->
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 403}
                               ])
      end ;
    _ ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"Devices Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}
                         ])
  end.



permissions() ->
  authorize_util:default_permission(?MODULE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_devices(Role, CustomerId, QueryJson, Limit, Offset) ->
  if 
    Role == ?USER_ROLE_ADMIN ->
       device_db:find_by_conditions([], [{<<"sort_created_time">>, desc} | QueryJson], Limit, Offset);
     Role == ?USER_ROLE_CUSTOMER ->
       device_db:find_by_conditions([{customer_id, CustomerId}], [{<<"sort_created_time">>, desc} | QueryJson], Limit, Offset);
     true -> []
  end.


% update_device(ReqJson, Device, CustomerId) ->
%   get_device_info(ReqJson, Device, CustomerId).

get_device_info(ReqJson, Device, CustomerId) ->
  AppId = wh_json:get_value(<<"app_id">>, ReqJson, maps:get(app_id, Device, <<>>)),
  PushId = wh_json:get_value(<<"push_id">>, ReqJson, maps:get(push_id, Device, <<>>)),
  Env = wh_json:get_value(<<"env">>, ReqJson, maps:get(env, Device, <<>>)),
  UpdateTime =  zt_datetime:get_now(),
  maps:merge(Device, #{
    app_id => AppId,
    push_id => PushId, 
    env => Env,
    created_by => CustomerId, 
    updated_by => CustomerId, 
    updated_time => UpdateTime
  }).

get_sub_fields_device(Role, DeviceInfo) ->
  WithoutFields = if  Role == ?USER_ROLE_ADMIN -> [];
                      Role == ?USER_ROLE_CUSTOMER -> [created_by, created_time,  updated_by, updated_time];
                      true  -> DeviceInfo
                  end,
  maps:without(WithoutFields, DeviceInfo).

validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?HTTP_PUT) ->
  Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]),
  ReqJson = cb_context:req_json(Context),
  Fun = [ 
        ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1, Fun);

validate_request(Context, _Verb) ->
  Context.

validate_request(_Id, Context, ?HTTP_GET) ->
  cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_POST) ->
  Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]),
  ReqJson = cb_context:req_json(Context),
  Fun = [ fun validate_update_customer_id/2,
          fun validate_update_env/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1, Fun);

validate_request(_Id, Context, _Verb) ->
  Context.

validate_update_customer_id(ReqJson, Context) ->
  CustomerId = wh_json:get_value(<<"customer_id">>, ReqJson, <<>>),
  if CustomerId /= <<>> ->
       api_util:validate_error(Context, <<"customer_id">>, <<"forbidden">>, <<"Not Allowed To Update CustomerId">>);
     true ->
       Context
  end.

% validate_update_status(ReqJson, Context) ->
%   Status = wh_json:get_value(<<"status">>, ReqJson),
%   lager:info("Status ~p ~n", [Status]),
%   if  Status == <<>>;
%     Status /= <<>> andalso Status /= undefined andalso Status /= ?INACTIVE ->
%       api_util:validate_error(Context, <<"status">>, <<"invalid">>, <<"status must be inactive">>);
%     true -> Context
%   end.

validate_update_env(ReqJson, Context) ->
  Env = wh_json:get_value(<<"env">>, ReqJson),
  if  Env == undefined -> Context;
      true ->
        case lists:member(Env, ?ENV) of
          true -> Context;
          _ ->
            api_util:validate_error(Context, <<"env">>, <<"invalid">>, <<"status must be dev/prod">>)

        end
  end.


add_device(DeviceId, ReqDeviceJson) ->
  Type = wh_json:get_value(<<"os_type">>, ReqDeviceJson, <<>>),
  add_device(DeviceId, ReqDeviceJson, Type).

add_device(_DeviceId, _ReqDeviceJson, <<"web">>) -> <<>>;

add_device(DeviceId, ReqDeviceJson, _) ->
  case device_db:find(DeviceId) of
    notfound ->
      create_device(DeviceId, ReqDeviceJson);
    Db when is_map(Db)  ->
      update_device(DeviceId, Db, ReqDeviceJson)
  end.

create_device(<<>>, ReqDeviceJson) ->
  lager:warning("push_device: nodevice"),
  DeviceId = zt_util:get_uuid(),
  create_device(DeviceId, ReqDeviceJson);

create_device(DeviceId, ReqDeviceJson) ->
  CreatedTime = zt_datetime:get_now(),
  AccountScope = wh_json:get_value(<<"account_scope">>, ReqDeviceJson, <<>>),
  AccountId = wh_json:get_value(<<"account_id">>, ReqDeviceJson, <<>>),
  AppId = wh_json:get_value(<<"app_id">>, ReqDeviceJson, <<>>),
  PushId = wh_json:get_value(<<"push_id">>, ReqDeviceJson, <<>>),
  OsType = wh_json:get_value(<<"os_type">>, ReqDeviceJson, <<>>),
  Doc = #{id => DeviceId,
          account_id => AccountId,
          account_scope => AccountScope,
          created_time => CreatedTime,
          updated_time => CreatedTime,
          app_id => AppId,
          os_type => OsType,
          push_id => PushId,
          status => <<"active">>
         },
  device_db:save(Doc),
  lager:info("push_device: created device ~p ~n", [Doc]),
  DeviceId.

update_device(DeviceId, DeviceDb, ReqDeviceJson) ->
  UpdatedTime = zt_datetime:get_now(),
  AccountScope = wh_json:get_value(<<"account_scope">>, ReqDeviceJson, maps:get(<<"account_scope">>, DeviceDb, <<>>)),
  AccountId = wh_json:get_value(<<"account_id">>, ReqDeviceJson, maps:get(<<"account_id">>, DeviceDb, <<>>)),
  AppId = wh_json:get_value(<<"app_id">>, ReqDeviceJson, maps:get(<<"app_id">>, DeviceDb, <<>>)),
  PushId = wh_json:get_value(<<"push_id">>, ReqDeviceJson, maps:get(<<"push_id">>, DeviceDb, <<>>)),
  OsType = wh_json:get_value(<<"os_type">>, ReqDeviceJson, maps:get(<<"os_type">>, DeviceDb, <<>>)),
  Doc = maps:merge(DeviceDb, #{app_id => AppId,
                               account_scope => AccountScope,
                               account_id => AccountId,
                               os_type => OsType,
                               push_id => PushId,
                               status => <<"active">>,
                               updated_time => UpdatedTime
                              }),
  device_db:save(Doc),
  lager:info("push_device: updated device ~p ~n", [Doc]),
  DeviceId.

update_status(DeviceDb, Status) ->
  lager:info("push_device: update_status Status ~p ~n", [Status]),
  UpdatedTime = zt_datetime:get_now(),
  Doc = maps:merge(DeviceDb, #{status => Status
                               ,updated_time => UpdatedTime}),
  device_db:save(Doc).



%---Add Client----%
add_device(CustomerId, AppId, PushId, OsType, Env, Status) ->
  Uuid = zt_util:get_uuid(),
  Id = <<"device", Uuid/binary>>,
  DeviceInfo = get_device_info(AppId, PushId, OsType, Env, Status),
  DeviceDb = maps:merge(DeviceInfo, #{id => Id,
                                      customer_id => CustomerId,
                                      created_by => CustomerId,
                                      updated_by => CustomerId}),
  device_db:save(DeviceDb),
  lager:info("CUSTOMER Device ~p ~n",[DeviceDb]),
  Id.

add_client(CustomerId, ClientId, OsType) ->
  Uuid = zt_util:get_uuid(),
  Id = <<"device", Uuid/binary>>,
  CreateTime = zt_datetime:get_now(),
  DeviceDb =  #{
                id => Id,
                client_id => ClientId,
                push_id => OsType,
                os_type => OsType,
                status => ?ACTIVE,
                customer_id => CustomerId,
                created_time => CreateTime,
                created_by => CustomerId,
                updated_by => CustomerId,
                updated_time => CreateTime
  },
  device_db:save(DeviceDb).

% add_client(CustomerId, ClientId, OsType) ->
%   Uuid = zt_util:get_uuid(),
%   Id = <<"device", Uuid/binary>>,
%   CreateTime = zt_datetime:get_now(),
%     DeviceDb =  #{id => Id,
%                   client_id => ClientId,
%                   push_id => OsType,
%                   os_type => OsType,
%                   status => ?ACTIVE,
%                   customer_id => CustomerId,
%                   created_time => CreateTime,
%                   created_by => CustomerId,
%                   updated_by => CustomerId,
%                   updated_time => CreateTime},
%     device_db:save(DeviceDb).




get_device_info(AppId, PushId, OsType, Env, Status) ->
  CreateTime = zt_datetime:get_now(),
  #{
    app_id => AppId, 
    push_id => PushId, 
    os_type => OsType, 
    env => Env, 
    status => Status,
    created_time => CreateTime, 
    updated_time => CreateTime
  }.