-module(cb_user).

-include("crossbar.hrl").

-define(PATH_CREATE, <<"create">>).
-define(PATH_RESEND, <<"resend">>).
-define(PATH_SEARCH, <<"search">>).

-define(PATH_PROFILE, <<"profile">>).
-define(PERMISSION_CREATE_USER, <<"create_staff">>).
-define(PERMISSION_CREATE_USER_DESC, {?PERMISSION_CREATE_USER, <<"User create other users">>}).
-define(PATH_SUGGEST, <<"suggest">>).
-define(PATH_MY_SUGGEST, <<"mysuggests">>).
-define(PATH_BOOKMARK, <<"bookmark">>).
-define(PATH_MYTASKS, <<"tasks">>).
-define(PATH_MYREQUESTS, <<"sos_requests">>).

-export([init/0
         ,allowed_methods/0
         ,allowed_methods/1
         ,allowed_methods/2
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
         ,handle_get/1
         ,handle_get/2
         ,handle_put/1
         ,handle_put/2
         ,handle_post/2
         ,handle_post/3]).

-export([
          errors/0,
          permissions/0,
          get_user_info/1
  ]).

init() ->
  _ = crossbar_bindings:bind(<<"*.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
  _ = crossbar_bindings:bind(<<"*.resource_exists.users">>, ?MODULE, 'resource_exists'),
  _ = crossbar_bindings:bind(<<"*.validate.users">>, ?MODULE, 'validate'),
  _ = crossbar_bindings:bind(<<"*.authenticate.users">>, ?MODULE, 'authenticate'),
  _ = crossbar_bindings:bind(<<"*.authorize.users">>, ?MODULE, 'authorize'),
  _ = crossbar_bindings:bind(<<"*.to_json.get.users">>, ?MODULE, 'handle_get'),
  _ = crossbar_bindings:bind(<<"*.execute.post.users">>, ?MODULE, 'handle_post'),
  _ = crossbar_bindings:bind(<<"*.execute.put.users">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().

%% /api/v1/users
allowed_methods() ->
  [?HTTP_GET, ?HTTP_PUT].

%% /api/v1/users/{id}
allowed_methods(_Path) ->
  [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT].

%% /api/v1/users/{id}/path
allowed_methods(_Id, _Path) ->
  [?HTTP_POST, ?HTTP_GET].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/users
resource_exists() -> 'true'.

%% /api/v1/users/{id}
resource_exists(?PATH_LOGOUT) -> 'true';

resource_exists(_Path) -> 'true'.

%% /api/v1/users/path
resource_exists(_Id, ?PATH_CONFIRM) -> 'true';
resource_exists(_Id, ?PATH_PASSWORD_CHANGE) -> 'true';
resource_exists(_Id, ?PATH_LOGOUT) -> 'true';

resource_exists(_Id, _Path) -> 'false'.

%% /api/v1/users
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  authenticate_with_method(Context, cb_context:req_verb(Context)).

authenticate_with_method(_Context, ?HTTP_PUT) -> true;

authenticate_with_method(Context, ?HTTP_GET) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/users/{id}
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context, ?PATH_CONFIRM) -> true;

authenticate(_Context, ?PATH_RESEND) -> true;

authenticate(Context, ?PATH_SEARCH) -> 

  Token = cb_context:auth_token(Context),
  case app_util:oauth2_authentic(Token, Context) of 
        false -> true;
        Res -> Res
  end;

authenticate(Context, _Id) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/users/{id}/path
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(_Context, _Id, ?PATH_CONFIRM) -> true;

authenticate(Context, _Id, ?PATH_PASSWORD_CHANGE) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(Context, _Id, ?PATH_LOGOUT) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(_Context, _Id, _Path) -> false. 

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) ->
  authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_PUT) -> true;

authorize_verb(Context, ?HTTP_GET) -> %Admin and opeator have permission to get list user
  Role = cb_context:role(Context),
  lists:member(Role, [?USER_ROLE_ADMIN,?USER_ROLE_OPERATOR]).

authorize(_Context, ?PATH_PROFILE = _Path) -> true;
authorize(_Context, ?PATH_RESEND = _Path) -> true;
authorize(_Context, ?PATH_CONFIRM = _Path) -> true;
authorize(_Context, ?PATH_SEARCH = _Path) -> true;
authorize(_Context, ?PATH_BOOKMARK = _Path) -> true;
authorize(_Context, ?PATH_SUGGEST = _Path) -> true;
authorize(_Context, ?PATH_MY_SUGGEST = _Path) -> true;
authorize(_Context, ?PATH_MYTASKS = _Path) -> true;
authorize(_Context, ?PATH_MYREQUESTS = _Path) -> true;

authorize(Context, Path) ->
  authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Id, ?HTTP_GET) -> true;

authorize_verb(Context, Id, ?HTTP_POST) -> % Update other role, create user
  Role = cb_context:role(Context),
  Role == ?USER_ROLE_ADMIN;

authorize_verb(Context, Id, ?HTTP_DELETE) -> 
  Role = cb_context:role(Context),
  Role == ?USER_ROLE_ADMIN.

authorize(_Context, _Id, _Path) ->
    true.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/users
validate(Context) ->
  validate_user(Context, cb_context:req_verb(Context)).


%% Validate resource : /api/v1/users/{id}
validate(Context, Id) ->
  validate_user(Context, Id, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/users/{id}/path
validate(Context, Id, Path) ->
  validate_user(Context, Id, Path, cb_context:req_verb(Context)).

-spec validate_user(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_user(cb_context:context(), path_token(), http_method()) -> cb_context:context().
-spec validate_user(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().

%% PUT /api/v1/users
validate_user(Context, ?HTTP_PUT = Verb) ->
  validate_request(Context, Verb);

%% GET /api/v1/users
validate_user(Context, ?HTTP_GET = Verb) ->
  validate_request(Context, Verb).

%% POST /api/v1/users/{id}
validate_user(Context, Path, ?HTTP_PUT = Verb) ->
  validate_request(Path, Context, Verb);

validate_user(Context, Id, ?HTTP_POST = Verb) ->
  validate_request(Id, Context, Verb);

%% GET /api/v1/users/{id}
validate_user(Context, Id, ?HTTP_GET = Verb) ->
  validate_request(Id, Context, Verb).

%% POST /api/v1/users/{id}/path
validate_user(Context, Id, Path, ?HTTP_POST = Verb) ->
  validate_request(Id, Context, Path, Verb);

validate_user(Context, _Id, _Path, _Verb) ->
  Context.


-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().
%% GET api/v1/users
handle_get({Req, Context}) ->
  QueryJson = cb_context:query_string(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
  Role = cb_context:role(Context),
  RequiredConds = 
    case Role of 
      ?USER_ROLE_ADMIN -> [];
      ?USER_ROLE_OPERATOR -> [{role,'in',[?USER_ROLE_OPERATOR,?USER_ROLE_USER]}];
      _ -> 
        UserId = cb_context:user_id(Context),
        [{user_id,UserId}]
    end,  
  Users = user_db:find_by_conditions(RequiredConds, PropQueryJson, Limit, Offset),
  PropUsers = 
    lists:map(fun(Account) ->
          get_sub_fields_users(Account) end, 
  Users),
  {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, PropUsers}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.

%% GET api/v1/users/{id}
handle_get({Req, Context}, ?PATH_MYTASKS) ->
  UserId = cb_context:user_id(Context),
  case user_db:find(UserId) of 
    #{} = UserInfo -> 
   
      Groups = group_handler:find_groups_by_user(UserId),
      GroupIds = 
        lists:map(fun(#{id := GroupId}) -> 
          GroupId
        end,Groups),
    lager:debug("GroupIds: ~p~n",[GroupIds]),
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
      SosRequests = 
                  sos_request_db:find_by_conditions([
                    {'or',[
                      {'and',[
                        {'or',[
                          {<<"supporters.type">>,?OBJECT_TYPE_USER},
                        {<<"supporters#type">>,?OBJECT_TYPE_USER}
                          ]},
                          {'or',[
                          {<<"supporters.id">>,UserId},
                        {<<"supporters#id">>,UserId}
                          ]}
                      ]},
                      {
                        'and',[
                          {<<"supporters.type">>,?OBJECT_TYPE_GROUP},
                          {<<"supporters.id">>,'in',GroupIds}
                      ]}
                  ]
              }], PropQueryJson, Limit, Offset),

    {Req, cb_context:setters(Context
                    ,[{fun cb_context:set_resp_data/2, SosRequests}
                    ,{fun cb_context:set_resp_status/2, 'success'}
           ])};
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      {Req, cb_context:setters(Context2, [
                  {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                  {fun cb_context:set_resp_status/2, <<"error">>},
                  {fun cb_context:set_resp_error_code/2, 404}
      ])}
  end;

handle_get({Req, Context}, ?PATH_MYREQUESTS) ->
  UserId = cb_context:user_id(Context),
  case user_db:find(UserId) of 
    #{} = UserInfo -> 
   
      Groups = group_handler:find_groups_by_user(UserId),
      GroupIds = 
        lists:map(fun(#{id := GroupId}) -> 
          GroupId
        end,Groups),
    lager:debug("UserId: ~p, GroupIds: ~p~n",[UserId, GroupIds]),
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
      SosRequests = 
                  sos_request_db:find_by_conditions([
                    {'or',[
                      {'and',[
                          {<<"requester_type">>,?OBJECT_TYPE_USER},
                          {<<"requester_info.id">>,UserId}
                      ]}
                      ,{
                        'and',[
                          {<<"requester_type">>,?OBJECT_TYPE_GROUP},
                          {<<"requester_info.id">>,'in',GroupIds}
                      ]}
                  ]
              }], PropQueryJson, Limit, Offset),

    {Req, cb_context:setters(Context
                    ,[{fun cb_context:set_resp_data/2, SosRequests}
                    ,{fun cb_context:set_resp_status/2, 'success'}
           ])};
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      {Req, cb_context:setters(Context2, [
                  {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                  {fun cb_context:set_resp_status/2, <<"error">>},
                  {fun cb_context:set_resp_error_code/2, 404}
      ])}
  end;

handle_get({Req, Context}, ?PATH_PROFILE) ->
  UserId = cb_context:user_id(Context),
  lager:debug("UserId: ~p~n",[UserId]),
  case user_db:find(UserId) of 
    #{} = UserInfo -> 
   
      FilteredGroups = group_handler:find_groups_by_user(UserId),
      PropUserInfo = get_sub_fields_users(UserInfo),
      RespData = maps:merge(PropUserInfo,#{
        groups => FilteredGroups
      }),
    {Req, cb_context:setters(Context
                    ,[{fun cb_context:set_resp_data/2, RespData}
                    ,{fun cb_context:set_resp_status/2, 'success'}
           ])};
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      {Req, cb_context:setters(Context2, [
                  {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                  {fun cb_context:set_resp_status/2, <<"error">>},
                  {fun cb_context:set_resp_error_code/2, 404}
      ])}
  end;

handle_get({Req, Context}, ?PATH_BOOKMARK) ->
  UserId = cb_context:user_id(Context),
  QueryJson = cb_context:query_string(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
          SosRequests = 
              sos_request_db:find_by_conditions([
                  {<<"bookmarks.bookmarker_type">>,?OBJECT_TYPE_USER},
                  {<<"bookmarks.bookmarker_id">>,UserId}
              ], PropQueryJson, Limit, Offset),
        {Req,
  cb_context:setters(Context,
                            [{fun cb_context:set_resp_data/2, SosRequests},
                             {fun cb_context:set_resp_status/2, success}])};

handle_get({Req, Context}, ?PATH_SUGGEST) ->
  UserId = cb_context:user_id(Context),
  QueryJson = cb_context:query_string(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
  Groups = group_handler:find_groups_by_user(UserId),

  GroupIds = 
    lists:map(fun(#{id := GroupId}) -> 
      GroupId
    end,Groups),
  lager:debug("GroupIds: ~p~n",[GroupIds]),
  SosRequests = 
              sos_request_db:find_by_conditions([
                {'or',[
                  {'and',[
                      {<<"suggests.target_type">>,?OBJECT_TYPE_USER},
                      {<<"suggests.target_id">>,UserId}
                  ]},
                  {
                    'and',[
                      {<<"suggests.target_type">>,?OBJECT_TYPE_GROUP},
                      {<<"suggests.target_id">>,'in',GroupIds}
                  ]}
                %   {'and',[
                %     {<<"suggests#target_type">>,?OBJECT_TYPE_USER},
                %     {<<"suggests#target_id">>,UserId}
                % ]},
                % {
                %   'and',[
                %     {<<"suggests#target_type">>,?OBJECT_TYPE_GROUP},
                %     {<<"suggests#target_id">>,'in',GroupIds}
                % ]}
              ]
          }], PropQueryJson, Limit, Offset),
        {Req,
         cb_context:setters(Context,
                            [{fun cb_context:set_resp_data/2, SosRequests},
                             {fun cb_context:set_resp_status/2, success}])};

    handle_get({Req, Context}, ?PATH_MY_SUGGEST) ->
        UserId = cb_context:user_id(Context),
        QueryJson = cb_context:query_string(Context),
        Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
        Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
        PropQueryJson = wh_json:to_proplist(QueryJson),
       
        SosRequests = 
            sos_request_db:find_by_conditions([{<<"suggests.suggester_id">>,UserId}], PropQueryJson, Limit, Offset),
        {Req,
              cb_context:setters(Context,
                    [{fun cb_context:set_resp_data/2, SosRequests},
                    {fun cb_context:set_resp_status/2, success}])};

handle_get({Req, Context}, Id) ->
  Role = cb_context:role(Context),	
  UserId = cb_context:user_id(Context),
  lager:debug("Logined  Get Id: ~p,  Logined Id: ~p~n",[Id, UserId]),
  
  ReqConds =
  case Role of 
      ?USER_ROLE_ADMIN -> [];
      ?USER_ROLE_OPERATOR -> 
              [{'or',[
                      {role,?USER_ROLE_USER},
                      {'and',[
                                {role,?USER_ROLE_OPERATOR},
                                {id,UserId}
                              ]}
                    ]
                }];
      ?USER_ROLE_USER -> [{id,UserId}]
  end,
  QueryJson = cb_context:query_string(Context),
  case user_db:find_by_conditions([{id,Id}|ReqConds],[{<<"sort_created_time">>, desc}|QueryJson],1,0) of 
    [UserInfo] -> 	
      UserInfoFiltered = get_sub_fields_users(UserInfo),
      {Req, cb_context:setters(Context
                             ,[{fun cb_context:set_resp_data/2, UserInfoFiltered}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                  ])};
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      {Req, cb_context:setters(Context2, [
              {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
              {fun cb_context:set_resp_status/2, <<"error">>},
              {fun cb_context:set_resp_error_code/2, 404}])}
  end. 


%% PUT api/v1/users
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->

  ReqJson = cb_context:req_json(Context),
  PhoneNumber  = zt_util:normalize_string(wh_json:get_value(<<"phone_number">>, ReqJson)),
  IsDebug =  wh_json:get_value(<<"debug">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  
  LoginedUserId = cb_context:user_id(Context),
  ConfirmCode = user_handler:create_confirm_code_by_phone(PhoneNumber),
  UserDb = 
    case user_handler:find_unconfirmed_user(PhoneNumber) of 
      notfound -> 
          Uuid = zt_util:get_uuid(),
          UserId = <<"user", Uuid/binary>>,
          BasicUserInfo = get_user_info(ReqJson),
          maps:merge(BasicUserInfo, 
                        #{
                            id => UserId,
                            first_name => FirstName, 
                            last_name => LastName,
                            status => ?USER_STATUS_UNCONFIRMED,
                            role => ?USER_ROLE_USER,
                            created_by => LoginedUserId,	
                            updated_by => LoginedUserId,
                            confirm_code => ConfirmCode,
                            confirm_code_created_time_dt => zt_datetime:get_now()
          });
      Info -> 
        maps:merge(Info, 
                        #{
                            confirm_code_created_time_dt => zt_datetime:get_now(),
                            confirm_code => ConfirmCode
                    })
    end,
  user_db:save(UserDb),
  user_actor:start_actor(PhoneNumber),
  InitRespData = maps:with([id,phone_number],UserDb),
  RespData = 
      case IsDebug of
            <<"true">> -> 
                maps:merge(InitRespData, #{
                    confirm_code => ConfirmCode
                });
            _ ->
                    user_handler:send_otp(PhoneNumber,ConfirmCode),
                    InitRespData
        end,
  cb_context:setters(Context
                        ,[{fun cb_context:set_resp_data/2, RespData}
                        ,{fun cb_context:set_resp_status/2, 'success'}]).

%% PUT api/v1/users/id
-spec handle_put(cb_context:context(), path_token()) -> cb_context:context().
handle_put(Context, ?PATH_CREATE) ->
  ReqJson = cb_context:req_json(Context),
  IsDebug =  wh_json:get_value(<<"debug">>, ReqJson),
  PhoneNumber  = wh_json:get_value(<<"phone_number">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  Role = wh_json:get_value(<<"role">>, ReqJson, ?USER_ROLE_USER),
  
  Uuid = zt_util:get_uuid(),
  ConfirmCode = zt_util:create_random_number(),
  UserInfo = get_user_info(ReqJson),
  UserBaseInfo = 
    case user_handler:find_unconfirmed_user(PhoneNumber) of 
      notfound -> 
          maps:merge(UserInfo, #{
                id => <<"user", Uuid/binary>>,
                first_name => FirstName, 
                last_name => LastName,
                status => ?USER_STATUS_UNCONFIRMED,
                role => Role,
                created_by => cb_context:user_id(Context),  
                updated_by => cb_context:user_id(Context),
                confirm_code => ConfirmCode,
                confirm_code_created_time_dt => zt_datetime:get_now()
          });
      Info -> Info
    end,
    UserDb = 
        maps:merge(UserBaseInfo,#{
            confirm_code_created_time_dt => zt_datetime:get_now(),
            confirm_code => ConfirmCode
        }),
  user_db:save(UserDb),
  InitRespData = maps:with([id,phone_number],UserDb),
  RespData = 
      case IsDebug of
        <<"true">> -> 
            maps:merge(InitRespData, #{
                confirm_code => ConfirmCode
            });
        _ ->
            user_handler:send_otp(PhoneNumber,ConfirmCode),
            InitRespData
      end,

  cb_context:setters(Context
                          ,[{fun cb_context:set_resp_data/2, RespData}
                            ,{fun cb_context:set_resp_status/2, 'success'}]);

handle_put(Context, _Id) ->
  ?MODULE:handle_put(Context).

%% POST api/v1/users/{id}
-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().

handle_post(Context, ?PATH_CONFIRM) ->
  ReqJson =  cb_context:req_json(Context),
  PhoneNumber  = wh_json:get_value(<<"phone_number">>, ReqJson),
  Result = user_db:find_by_phone_number(PhoneNumber),
  user_handler:handle_user_confirm(Context, Result);

handle_post(Context, ?PATH_RESEND) ->
  ReqJson =  cb_context:req_json(Context),
  PhoneNumber  = wh_json:get_value(<<"phone_number">>, ReqJson),
  Result = user_db:find_by_phone_number(PhoneNumber),
  user_handler:handle_user_resend(Context, Result);
  

handle_post(Context, ?PATH_PROFILE) ->
  ReqJson =  cb_context:req_json(Context),
  UserId = cb_context:user_id(Context),
  case user_db:find(UserId) of 
    #{
      first_name := FirstnameDb,
      last_name := LastnameDb,
      phone_number := PhoneNumberDb,
      address := AddressDb, 
      avatar := AvatarDb
    } = UserInfo -> 
          Address = wh_json:get_value(<<"address">>, ReqJson, AddressDb),
          Avatar = wh_json:get_value(<<"avatar">>, ReqJson, AvatarDb), 
          Firstname = wh_json:get_value(<<"first_name">>, ReqJson, FirstnameDb), 
          Lastname = wh_json:get_value(<<"last_name">>, ReqJson, LastnameDb), 

          NewUserInfo = maps:merge(UserInfo, #{
                        address => Address,
                        avatar => Avatar, 
                        first_name => Firstname,
                        last_name => Lastname,
                        updated_time_dt => zt_datetime:get_now(),
                        updated_by => UserId
                      }),
          user_db:save(NewUserInfo),
          RespData = get_sub_fields_users(NewUserInfo),
          cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
                                       ,{fun cb_context:set_resp_status/2, 'success'}]);
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}])
  end;

handle_post(Context, ?PATH_PASSWORD_CHANGE) ->  
  UserId = cb_context:user_id(Context),
  ReqJson =  cb_context:req_json(Context),
  CurrPass  = wh_json:get_value(<<"current_password">>, ReqJson), 
  NewPass = wh_json:get_value(<<"new_password">>, ReqJson),
  Info = user_db:find(UserId),
  #{
    password := CurrPassHashServer
  } = Info,
  PassHash = zt_util:to_str(CurrPassHashServer),
  {ok, ProvidedHash} = bcrypt:hashpw(CurrPass, PassHash),
  if  
      ProvidedHash == PassHash ->
          {ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
          {ok, NewPassHash} = bcrypt:hashpw(NewPass, Salt),
          UpdatedUserDB = 
                maps:merge(Info, #{
                        password => zt_util:to_bin(NewPassHash), 
                        updated_time_dt =>  zt_datetime:get_now(),
                        updated_by =>  UserId
                 }),
          user_db:save(UpdatedUserDB),
          api_doc:del_tokens_of_user(UserId),
          RespData = [{<<"id">>, UserId}],
          cb_context:setters(Context,[
                      {fun cb_context:set_resp_data/2, RespData}
                      ,{fun cb_context:set_resp_status/2, 'success'}
                    ]);
      true ->
          Context2 = api_util:validate_error(Context, <<"current_password">>, <<"invalid">>, <<"current_password_invalid">>), 
          cb_context:setters(Context2,
                                     [{fun cb_context:set_resp_error_msg/2, <<"Invalid Current Password">>},
                                      {fun cb_context:set_resp_status/2, <<"error">>},
                                      {fun cb_context:set_resp_error_code/2, 400}
                                     ])

  end;

handle_post(Context, ?PATH_SEARCH) ->  
ReqJson =  cb_context:req_json(Context),
UserId = cb_context:user_id(Context),
PhoneNumber  = zt_util:normalize_string(wh_json:get_value(<<"phone_number">>, ReqJson)),
case user_db:find_by_phone_number(PhoneNumber) of 
[UserInfo] -> 
    
    RespData  =
        case zt_util:nvl(UserId, <<>>) of 
          <<>> -> #{
                    phone_number => PhoneNumber,
                    is_existed => true
                  };
          _ -> 
            maps:with([id,first_name,last_name, phone_number,address,avatar],UserInfo)
        end,
    cb_context:setters(Context,[
      {fun cb_context:set_resp_data/2, RespData}
      ,{fun cb_context:set_resp_status/2, 'success'}
    ]);
 _ -> 
        case zt_util:nvl(UserId, <<>>) of 
          <<>> -> 
            RespData = #{
                    phone_number => PhoneNumber,
                    is_existed => false
                  },
              cb_context:setters(Context,[
                {fun cb_context:set_resp_data/2, RespData}
                ,{fun cb_context:set_resp_status/2, 'success'}
              ]);
          _ -> 
          Context2 = api_util:validate_error(Context, <<"phone_number">>, <<"not_found">>, <<"phone_number_notfound">>), 
          cb_context:setters(Context2,
                                      [{fun cb_context:set_resp_error_msg/2, <<"Phone Number Notfound">>},
                                      {fun cb_context:set_resp_status/2, <<"error">>},
                                      {fun cb_context:set_resp_error_code/2, 404}
                                      ]) 

          end
end;

handle_post(Context, ?PATH_LOGOUT) ->
  UserId = cb_context:user_id(Context),
  Role = cb_context:role(Context),
  ReqJson =  cb_context:req_json(Context),
  Token = cb_context:auth_token(Context),
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
  Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
  lager:debug("Role: ~p~n",[Role]),
  access_token_mnesia_db:del_by_token(Token),
  refresh_token_db:del_by_token(Token),
  RespData = [{token, Token}],
  cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData}
                                           ,{fun cb_context:set_resp_status/2, 'success'}]);
  
handle_post(Context, Id) ->
  ReqJson =  cb_context:req_json(Context),
  UserRole = cb_context:role(Context),
  UserId = cb_context:user_id(Context),
  case user_db:find(Id) of 
    #{
      role := RoleDb,
      address := AddressDb,
      avatar := AvatarDb
    } = Info ->           
          {IsChangedRole, Role} = 
          case wh_json:get_value(<<"role">>, ReqJson, <<>>) of 
            <<>> -> {false, RoleDb};
            NewRole -> 
              case lists:member(NewRole, ?USER_ROLES) of 
                true -> {true, NewRole};
                _ -> {false, RoleDb}
              end
          end,
          NewUserDb = maps:merge(Info, #{
            role => Role,
            address => wh_json:get_value(<<"address">>, ReqJson, AddressDb),
            avatar => wh_json:get_value(<<"avatar">>, ReqJson, AvatarDb), 
            updated_time_dt => zt_datetime:get_now(), 
            updated_by => UserId
          }),
          user_db:save(NewUserDb),
          spawn(fun() -> 
            user_handler:maybe_update_role_token(IsChangedRole, Id, Role)
          end),
          RespData = get_sub_fields_users(NewUserDb),
          cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
                                       ,{fun cb_context:set_resp_status/2, 'success'}]);
    _ ->
      Context2 = api_util:validate_error(Context, <<"user">>, <<"not_found">>, <<"user_notfound">>), 
      cb_context:setters(Context2,
                         [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}])
  end.

%% POST api/v1/users/{id}/confirm
%TODO 
handle_post(Context, UserId, ?PATH_CONFIRM) ->
  UserInfo = user_db:find(UserId),
  user_handler:handle_user_confirm(Context, UserInfo);

%% POST api/v1/users/{id}/change_password
handle_post(Context, _Id, _Path) ->
  Context.

permissions() ->
  authorize_util:default_permission(?MODULE).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

%% PUT api/v1/users
validate_request(Context, ?HTTP_PUT) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
  ValidateFuns = [
                 fun user_handler:validate_phone_number/2
                ,fun user_handler:validate_password/2
              ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%%GET api/v1/users
validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, _Verb) ->
  Context.

%%GET api/v1/users/{id}
-spec  validate_request(path_token(), cb_context:context(), http_method()) -> cb_context:context().
validate_request(_Id, Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(?PATH_CREATE, Context, ?HTTP_PUT) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context
          ,[{fun cb_context:set_resp_status/2, 'success'}]),	
                              
    ValidateFuns = [ 
      fun user_handler:validate_phone_number/2
     ,fun user_handler:validate_role/2
     ,fun user_handler:validate_password/2
    ],
                  
  lists:foldl(fun(F, C) ->
        F(ReqJson, C)
  end, Context1,  ValidateFuns);

validate_request(?PATH_CONFIRM, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context
                    ,[{fun cb_context:set_resp_status/2, 'success'}]),	
            
    ValidateFuns = [ 
      fun user_handler:validate_confirm_phone_number/2,
      fun user_handler:validate_confirm_code/2
    ],

    lists:foldl(fun(F, C) ->
        F(ReqJson, C)
    end, Context1,  ValidateFuns);

  validate_request(?PATH_RESEND, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context
                    ,[{fun cb_context:set_resp_status/2, 'success'}]),	
            
    ValidateFuns = [ 
      fun user_handler:validate_confirm_phone_number/2
    ],

    lists:foldl(fun(F, C) ->
        F(ReqJson, C)
    end, Context1,  ValidateFuns);

  validate_request(?PATH_SEARCH, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context
                    ,[{fun cb_context:set_resp_status/2, 'success'}]),	
            
    ValidateFuns = [ 
      fun user_handler:validate_search_phone_number/2
    ],

    lists:foldl(fun(F, C) ->
        F(ReqJson, C)
    end, Context1,  ValidateFuns);

validate_request(_Id, Context, ?HTTP_POST) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [ 
                   fun user_handler:validate_update_password/2
                   ,fun user_handler:validate_update_timezone/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_Id, Context, _Verb) ->
  Context.


%% POST api/v1/users/{id}/confirm
-spec  validate_request(path_token(), cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_request(_Id, Context, ?PATH_CONFIRM, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun user_handler:validate_confirm_code/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%%%%POST api/v1/users/{id}/password_change
validate_request(_Id, Context, ?PATH_PASSWORD_CHANGE, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun user_handler:validate_curr_password/2
                  ,fun user_handler:validate_new_password/2],

  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_Id, Context, ?PATH_LOGOUT, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
  ReqJson =  cb_context:req_json(Context1),
  ValidateFuns = [
  ],                        
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_Id, Context, _Path, _Verb) ->
  Context.

get_user_info(ReqJson) ->
  Email  = wh_json:get_value(<<"email">>, ReqJson,<<>>), 
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  Address = wh_json:get_value(<<"address">>, ReqJson, <<>>),
  Role = wh_json:get_value(<<"role">>, ReqJson, ?USER_ROLE_USER),
  Avatar = wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, <<>>),
  Password = wh_json:get_value(<<"password">>, ReqJson,zt_util:get_uuid()),
  {ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
  {ok, HashPass} = bcrypt:hashpw(Password, Salt),
  CreatedTime = zt_datetime:get_now(),
  #{
      email => Email, 
      phone_number => PhoneNumber, 
      first_name => FirstName, 
      last_name => LastName,
      address => Address, 
      password => zt_util:to_bin(HashPass), 
      role => Role, 
      avatar => Avatar, 
      time_zone => TimeZone, 
      created_time_dt => CreatedTime,
      updated_time_dt => CreatedTime
      
    }.

get_sub_fields_users(User) -> 
  Fields = [
      roles,
      account_id,
      password, 
      created_by,
      created_time_dt,
      updated_by,
      updated_time_dt,
      confirm_code, 
      confirm_code_created_time_dt
  ] ,
  maps:without(Fields, User).

errors() -> 
  Path = <<"users">>,
  HandlePutValidates =
  [
    {<<"phone_number">>, <<"required">>, <<"phone_number_required">>},
    {<<"phone_number">>, <<"invalid">>, <<"phone_number_is_invalid">>},
    {<<"phone_number">>, <<"invalid">>, <<"phone_number_in_use">>},
    {<<"password">>, <<"required">>, <<"password_required">>},
    {<<"password">>, <<"invalid">>, <<"password_min_8_charactor">>}  
  ],
  HandlePut = app_util:declare_api_validate(<<"put">>,Path,HandlePutValidates),

  Path1GetId = <<"users/{id}">>,
  HandleGet1Validates =
  [
    {<<"user">>, <<"not_found">>, <<"user_notfound">>},
    {<<"permission">>, <<"invalid">>, <<"forbidden">>}  

  ],
  HandleGet1Id = app_util:declare_api_validate(<<"get">>,Path1GetId,HandleGet1Validates),


  PathConfirm = ?PATH_CONFIRM,
  Path1Confirm = <<Path/binary,"/",PathConfirm/binary>>,
  HandlePost1ConfirmValidates =
  [
    {<<"phone_number">>, <<"required">>, <<"phone_number_required">>},
    {<<"confirm_code">>, <<"required">>, <<"confirm_code_required">>},
    {<<"confirm_code">>, <<"invalid">>, <<"confirm_code_expired">>},
    {<<"confirm_code">>, <<"invalid">>, <<"confirm_code_not_match">>},
    {<<"phone_number">>, <<"invalid">>, <<"phon_number_notfound">>}

  ],
  HandlePost1Confirm = app_util:declare_api_validate(<<"post">>,Path1Confirm,HandlePost1ConfirmValidates),

  PathResend = ?PATH_RESEND,
  Path1Resend = <<Path/binary,"/",PathResend/binary>>,
  HandlePost1ResendValidates =
  [
    {<<"phone_number">>, <<"required">>, <<"phone_number_required">>},
    {<<"phone_number">>, <<"invalid">>, <<"phon_number_notfound">>},
    {<<"resend_otp">>, <<"invalid">>, <<"phon_number_notfound">>},
    {<<"resend_otp">>, <<"invalid">>, <<"resend_too_fast">>},
    {<<"resend_otp">>, <<"invalid">>, <<"max_resend_reached">>}
  ],
  HandlePost1Resend = app_util:declare_api_validate(<<"post">>,Path1Resend, HandlePost1ResendValidates),


  PathPasswordChange = ?PATH_PASSWORD_CHANGE,
  Path1PasswordChange = <<Path/binary,"/",PathPasswordChange/binary>>,
  HandlePost1PasswordChangeValidates =
  [
    {<<"current_password">>, <<"required">>, <<"Field 'current_password' is required">>},
    {<<"new_password">>, <<"required">>, <<"Field 'new_password' is required">>},
    {<<"new_password">>, <<"invalid">>, <<"password_min_8_charactor">>},
    {<<"user">>, <<"not_found">>, <<"user_notfound">>},
    {<<"permission">>, <<"invalid">>, <<"forbidden">>} 

  ],
  HandlePost1PasswordChange = app_util:declare_api_validate(<<"post">>,Path1PasswordChange, HandlePost1PasswordChangeValidates),

  PathUpdateProfile = ?PATH_PROFILE,
  Path1UpdateProfile = <<Path/binary,"/",PathUpdateProfile/binary>>,
  HandlePost1UpdateProfileValidates =
  [
    {<<"user">>, <<"not_found">>, <<"user_notfound">>} 

  ],
  HandlePost1UpdateProfile = app_util:declare_api_validate(<<"post">>,Path1UpdateProfile, HandlePost1UpdateProfileValidates),

  Apis = [
    HandlePut,
    HandleGet1Id,
    HandlePost1Confirm,
    HandlePost1Resend,
    HandlePost1PasswordChange,
    HandlePost1UpdateProfile
  ],
  app_util:create_module_validates(Apis).