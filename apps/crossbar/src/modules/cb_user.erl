-module(cb_user).

-include("crossbar.hrl").

-define(PATH_CREATE, <<"create">>).
-define(PATH_RESEND, <<"resend">>).

-define(PATH_PROFILE, <<"profile">>).
-define(PERMISSION_CREATE_USER, <<"create_staff">>).
-define(PERMISSION_CREATE_USER_DESC, {?PERMISSION_CREATE_USER, <<"User create other users">>}).

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
          permissions/0
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

%% /api/v1/account
allowed_methods() ->
  [?HTTP_GET, ?HTTP_PUT].

%% /api/v1/account/accountid
allowed_methods(_Path) ->
  [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT].

%% /api/v1/account/accountid/path
allowed_methods(_AccountId, _Path) ->
  [?HTTP_POST].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/account
resource_exists() -> 'true'.

%% /api/v1/account/accountid
resource_exists(?PATH_LOGOUT) -> 'true';

resource_exists(_Path) -> 'true'.

%% /api/v1/account/path
resource_exists(_Id, ?PATH_CONFIRM) -> 'true';
resource_exists(_Id, ?PATH_PASSWORD_CHANGE) -> 'true';
resource_exists(_Id, ?PATH_LOGOUT) -> 'true';
resource_exists(_Id, _Path) -> 'false'.

%% /api/v1/account
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  authenticate_with_method(Context, cb_context:req_verb(Context)).

authenticate_with_method(_Context, ?HTTP_PUT) -> true;

authenticate_with_method(Context, ?HTTP_GET) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/account/accountid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context, ?PATH_CONFIRM) -> true;
authenticate(_Context, ?PATH_RESEND) -> true;
authenticate(Context, _Path) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/account/accountid/path
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(_Context, _AccountId, ?PATH_CONFIRM) -> true;

authenticate(Context, _AccountId, ?PATH_PASSWORD_CHANGE) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(Context, _AccountId, ?PATH_LOGOUT) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(_Context, _AccountId, _Path) -> false. 

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) ->
  authorize_util:authorize(?MODULE, Context).

authorize(Context, ?PATH_CREATE = _Path) ->
    authorize_util:authorize(?MODULE, Context, {permission, ?PERMISSION_CREATE_USER}); 

authorize(_Context, ?PATH_PROFILE = _Path) -> true;

authorize(_Context, _Id) ->
    true. 

authorize(_Context, _Id, _Path) ->
    true.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/account
validate(Context) ->
  validate_user(Context, cb_context:req_verb(Context)).


%% Validate resource : /api/v1/account/accountid
validate(Context, AccountId) ->
  validate_user(Context, AccountId, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/account/accountid/path
validate(Context, AccountId, Path) ->
  validate_user(Context, AccountId, Path, cb_context:req_verb(Context)).

-spec validate_user(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_user(cb_context:context(), path_token(), http_method()) -> cb_context:context().
-spec validate_user(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().

%% PUT /api/v1/account
validate_user(Context, ?HTTP_PUT = Verb) ->
  validate_request(Context, Verb);

%% GET /api/v1/account
validate_user(Context, ?HTTP_GET = Verb) ->
  validate_request(Context, Verb).

%% POST /api/v1/account/accountid
validate_user(Context, Path, ?HTTP_PUT = Verb) ->
  validate_request(Path, Context, Verb);

validate_user(Context, AccountId, ?HTTP_POST = Verb) ->
  validate_request(AccountId, Context, Verb);

%% GET /api/v1/account/accountid
validate_user(Context, AccountId, ?HTTP_GET = Verb) ->
  validate_request(AccountId, Context, Verb).

%% POST /api/v1/account/accountid/path
validate_user(Context, AccountId, Path, ?HTTP_POST = Verb) ->
  validate_request(AccountId, Context, Path, Verb);

validate_user(Context, _AccountId, _Path, _Verb) ->
  Context.


-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().
%% GET api/v1/account
handle_get({Req, Context}) ->
  QueryJson = cb_context:query_string(Context),
  AccountId = cb_context:account_id(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
  Users = user_db:find_by_conditions([{account_id, AccountId}], PropQueryJson, Limit, Offset),
  PropUsers = lists:map(fun(Account) ->
          get_sub_fields_accounts(Account) end, 
      Users),
  {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, PropUsers}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.

%% GET api/v1/account/accountid
handle_get({Req, Context}, ?PATH_PROFILE) ->
  UserId = cb_context:user_id(Context),
  lager:debug("UserId: ~p~n",[UserId]),
  case user_db:find(UserId) of 
    #{} = UserInfo ->   
    PropUserInfo = get_sub_fields_accounts(UserInfo),
    {Req, cb_context:setters(Context
                                   ,[{fun cb_context:set_resp_data/2, PropUserInfo}
                                     ,{fun cb_context:set_resp_status/2, 'success'}
                                    ])};
    _ ->
      {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])}
  end;

handle_get({Req, Context}, Id) ->
  AccountId = cb_context:account_id(Context),
  Role = cb_context:role(Context),	
  
  lager:debug("Logined 2 account: ~p, Get Id: ~p,  Logined Id: ~p~n",[AccountId, Id, cb_context:user_id(Context)]),
  case user_db:find(Id) of 
    #{account_id := AccountIdDb} = AccountInfo -> 	
      if 
        Role == ?USER_ROLE_ADMIN; 
        Role == ?USER_ROLE_USER andalso AccountIdDb == AccountId ->
          PropAccount = get_sub_fields_accounts(AccountInfo),
          {Req, cb_context:setters(Context
                                   ,[{fun cb_context:set_resp_data/2, PropAccount}
                                     ,{fun cb_context:set_resp_status/2, 'success'}
                                    ])};
        true ->
          {Req, cb_context:setters(Context,
                                   [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                    {fun cb_context:set_resp_status/2, <<"error">>},
                                    {fun cb_context:set_resp_error_code/2, 403}
                                   ])}
      end;
    _ ->
      {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])}
  end. 


%% PUT api/v1/account
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->

  ReqJson = cb_context:req_json(Context),
  PhoneNumber  = zt_util:normalize_string(wh_json:get_value(<<"phone_number">>, ReqJson)),
  %IsDebug =  wh_json:get_value(<<"debug">>, ReqJson),
  IsDebug = <<"true">>,
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
                            created_by => LoginedUserId,	
                            updated_by => LoginedUserId,
                            confirm_code => ConfirmCode
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

%% PUT api/v1/account/accountid
-spec handle_put(cb_context:context(), path_token()) -> cb_context:context().
handle_put(Context, _AccountId) ->
  ?MODULE:handle_put(Context).

%% POST api/v1/account/accountid
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
          PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, PhoneNumberDb),
          Address = wh_json:get_value(<<"address">>, ReqJson, AddressDb),
          Avatar = wh_json:get_value(<<"avatar">>, ReqJson, AvatarDb), 
          Firstname = wh_json:get_value(<<"first_name">>, ReqJson, FirstnameDb), 
          Lastname = wh_json:get_value(<<"last_name">>, ReqJson, LastnameDb), 

          NewUserInfo = maps:merge(UserInfo, #{phone_number => PhoneNumber, 
                        address => Address,
                        avatar => Avatar, 
                        first_name => Firstname,
                        last_name => Lastname,
                        updated_time_dt => zt_datetime:get_now(),
                        updated_by => UserId
                      }),
          user_db:save(NewUserInfo),
          RespData = get_sub_fields_accounts(NewUserInfo),
          cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
                                       ,{fun cb_context:set_resp_status/2, 'success'}]);
    _ ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}])
  end;

handle_post(Context, ?PATH_PASSWORD_CHANGE) ->  
  UserId = cb_context:user_id(Context),
  handle_post(Context, UserId, ?PATH_PASSWORD_CHANGE);

handle_post(Context, ?PATH_LOGOUT) ->
  UserId = cb_context:user_id(Context),
  handle_post(Context, UserId, ?PATH_LOGOUT);
  
handle_post(Context, Id) ->
  ReqJson =  cb_context:req_json(Context),
  AccountId = cb_context:account_id(Context),
  case user_db:find(Id) of 
    #{time_zone := TimeZoneDb, roles := RolesDb, account_id := AccountIdDb} = AccountInfo -> 
          TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, TimeZoneDb),
          UpdateTime =  zt_datetime:get_now(),
          
          UpdatedUserDB = get_user_info(ReqJson, AccountInfo, Id, TimeZone, UpdateTime),
          RolesPropsList = wh_json:get_value(<<"roles">>, ReqJson, []),
          
          {IsChangedRoles, RolesMapList} = 
          case RolesPropsList of 
            [] -> {false, RolesDb};
            _ -> 
            {true, 
              lists:map(fun(RoleProps) -> 
                maps:from_list(RoleProps)
              end, RolesPropsList)
              }
          end,
          NewUserDb = maps:merge(UpdatedUserDB, #{
            roles => RolesMapList
          }),
          user_db:save(NewUserDb),
          spawn(fun() -> 
            maybe_update_role_token(IsChangedRoles, Id, RolesMapList)
          end),
          RespData = get_sub_fields_accounts(NewUserDb),
          cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
                                       ,{fun cb_context:set_resp_status/2, 'success'}]);
    _ ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}])
  end.


maybe_update_role_token(true, UserId, NewRoles) -> 
  AccessTokens = access_token_mnesia_db:find_by_user_id(UserId),
  lists:foreach(fun(AccessTokenInfo) -> 
    NewAccessTokenInfo = maps:merge(AccessTokenInfo, #{
      roles => NewRoles
    }),
    access_token_mnesia_db:save(NewAccessTokenInfo)
  end, AccessTokens),
  %TODO with refresh_token
ok;
maybe_update_role_token(false, _UserId, _NewRoles) -> ok.

%% POST api/v1/users/{id}/confirm
%TODO 
handle_post(Context, UserId, ?PATH_CONFIRM) ->
  UserInfo = user_db:find(UserId),
  user_handler:handle_user_confirm(Context, UserInfo);

%% POST api/v1/users/{id}/change_password
%TODO 
handle_post(Context, Id, ?PATH_PASSWORD_CHANGE) ->	
  Role = cb_context:role(Context), 
  case user_db:find(Id) of 
    #{
        id := UserIdDb, 
        password := CurrPassHashServer
      } = Info -> 
      if 	Role == ?USER_ROLE_ADMIN; 
          Role == ?USER_ROLE_USER andalso Id == UserIdDb -> 	
            ReqJson =  cb_context:req_json(Context),
            CurrPass  = wh_json:get_value(<<"current_password">>, ReqJson), 
            NewPass = wh_json:get_value(<<"new_password">>, ReqJson),
            PassHash = zt_util:to_str(CurrPassHashServer),
            {ok, ProvidedHash} = bcrypt:hashpw(CurrPass, PassHash),
            if  ProvidedHash == PassHash ->
                  UpdatedTime = zt_datetime:get_now(),
                  {ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
                  {ok, NewPassHash} = bcrypt:hashpw(NewPass, Salt),
                  UpdatedUserDB = maps:merge(Info, #{
                        password => zt_util:to_bin(NewPassHash), 
                        updated_time_dt =>  UpdatedTime,
                        updated_by =>  UserIdDb
                  }),
                  user_db:save(UpdatedUserDB),
                  api_doc:del_tokens_of_user(Id),
                  RespData = [{<<"id">>, Id}],
                  cb_context:setters(Context
                                     ,[{fun cb_context:set_resp_data/2, RespData}
                                       ,{fun cb_context:set_resp_status/2, 'success'}
                                      ]);
                true ->
                  cb_context:setters(Context,
                                     [{fun cb_context:set_resp_error_msg/2, <<"Invalid Current Password">>},
                                      {fun cb_context:set_resp_status/2, <<"error">>},
                                      {fun cb_context:set_resp_error_code/2, 400}
                                     ])

            end;
          true ->
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 403}
                               ])
      end;
    _ -> 
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}
                         ])
  end;


%% POST api/v1/account/accountid/logout
handle_post(Context, Id, ?PATH_LOGOUT) ->
  Role = cb_context:role(Context),
  AccountIdDb = cb_context:account_id(Context),
  UserId = cb_context:user_id(Context),
  ReqJson =  cb_context:req_json(Context),
  Token = cb_context:auth_token(Context),
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
  Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
  lager:debug("Role: ~p~n",[Role]),
  if  Role == ?USER_ROLE_ADMIN;
      Role == ?USER_ROLE_USER andalso Id == UserId->
              access_token_mnesia_db:del_by_token(Token),
              refresh_token_db:del_by_token(Token),
              RespData = [{token, Token}],
              cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData}
                                           ,{fun cb_context:set_resp_status/2, 'success'}]);	
      true -> 
        cb_context:setters(Context,
                           [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                            {fun cb_context:set_resp_status/2, <<"error">>},
                            {fun cb_context:set_resp_error_code/2, 403}])
  end;

%% POST api/v1/account/accountid/change_password
handle_post(Context, _AccountId, _Path) ->
  Context.

permissions() ->
  DefaultPermissions = authorize_util:default_permission(?MODULE),
  DefaultPermissions.
  %DefaultPermissions ++ [?PERMISSION_CREATE_USER_DESC].
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

%% PUT api/v1/account
validate_request(Context, ?HTTP_PUT) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
  ValidateFuns = [
    %fun validate_email/2
                 fun user_handler:validate_phone_number/2
                  ,fun user_handler:validate_password/2
                 ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%%GET api/v1/account
validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, _Verb) ->
  Context.

%%GET api/v1/account/accountid
-spec  validate_request(path_token(), cb_context:context(), http_method()) -> cb_context:context().
validate_request(_AccountId, Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);


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

validate_request(_AccountId, Context, ?HTTP_POST) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [ 
                  % ,fun validate_update_phone_number/2
                   fun user_handler:validate_update_password/2
                   ,fun user_handler:validate_update_timezone/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_AccountId, Context, _Verb) ->
  Context.


%% POST api/v1/account/accountid/confirm
-spec  validate_request(path_token(), cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_request(_AccountId, Context, ?PATH_CONFIRM, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun user_handler:validate_confirm_code/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%%%%POST api/v1/account/accountid/password_change
validate_request(_AccountId, Context, ?PATH_PASSWORD_CHANGE, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun user_handler:validate_curr_password/2
                  ,fun user_handler:validate_new_password/2],

  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%% POST api/v1/account/accountid/logout
validate_request(_AccountId, Context, ?PATH_LOGOUT, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
  ReqJson =  cb_context:req_json(Context1),
  ValidateFuns = [
  ],                        
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_AccountId, Context, _Path, _Verb) ->
  Context.


get_users(QueryJson, Limit, Offset) ->
        user_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).

get_user_info(ReqJson) ->
  Email  = wh_json:get_value(<<"email">>, ReqJson,<<>>), 
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  Address = wh_json:get_value(<<"address">>, ReqJson, <<>>),
  Role = wh_json:get_value(<<"role">>, ReqJson, ?USER_ROLE_USER),
  Avatar = wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, <<>>),
  Password = wh_json:get_value(<<"password">>, ReqJson),
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

get_user_info(ReqJson, Account, UserId, TimeZone, UpdateTime) ->
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, maps:get(phone_number, Account, <<>>)),
  Address = wh_json:get_value(<<"address">>, ReqJson, maps:get(address, Account, <<>>)),
  Role = wh_json:get_value(<<"role">>, ReqJson, maps:get(role, Account, <<>>)),
  Avatar = wh_json:get_value(<<"avatar">>, ReqJson, maps:get(avatar, Account, <<>>)), 
  maps:merge(Account, #{phone_number => PhoneNumber, 
                        address => Address,
                        role => Role, 
                        avatar => Avatar, 
                        time_zone => TimeZone, 
                        updated_time_dt => UpdateTime, 
                        updated_by => UserId
                      }).

get_sub_fields_accounts(User) -> 
  Fields = [roles,account_id, password, created_by, created_time_dt, updated_by, updated_time_dt,
            confirm_code, confirm_code_created_time_dt] ,
  NewMap = maps:without(Fields, User),
  Res = maps:to_list(NewMap),
  proplists:substitute_aliases([], Res).

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


  Apis = [
    HandlePut,
    HandlePost1Confirm,
    HandlePost1Resend
    
  ],
  app_util:create_module_validates(Apis).