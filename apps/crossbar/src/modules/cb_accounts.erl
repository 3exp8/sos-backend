-module(cb_accounts).

-include("crossbar.hrl").

-define(PATH_CREATE, <<"create">>).
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
        % ,handle_put/2
         ,handle_post/2
         ,handle_post/3]).

-export([
          permissions/0
  ]).

init() ->
  _ = crossbar_bindings:bind(<<"*.allowed_methods.accounts">>, ?MODULE, 'allowed_methods'),
  _ = crossbar_bindings:bind(<<"*.resource_exists.accounts">>, ?MODULE, 'resource_exists'),
  _ = crossbar_bindings:bind(<<"*.validate.accounts">>, ?MODULE, 'validate'),
  _ = crossbar_bindings:bind(<<"*.authenticate.accounts">>, ?MODULE, 'authenticate'),
  _ = crossbar_bindings:bind(<<"*.authorize.accounts">>, ?MODULE, 'authorize'),
  _ = crossbar_bindings:bind(<<"*.to_json.get.accounts">>, ?MODULE, 'handle_get'),
  _ = crossbar_bindings:bind(<<"*.execute.post.accounts">>, ?MODULE, 'handle_post'),
  _ = crossbar_bindings:bind(<<"*.execute.put.accounts">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().

%% /api/v1/account
allowed_methods() ->
  [?HTTP_PUT].
  %[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/account/accountid
allowed_methods(_Path) ->
  [].
 % [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT].

%% /api/v1/account/accountid/path
allowed_methods(_AccountId, _Path) ->
  [].
  %[?HTTP_POST].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/account
resource_exists() -> 'true'.

%% /api/v1/account/accountid
resource_exists(_Path) -> 'true'.

%% /api/v1/account/path
resource_exists(_AccountId, ?CONFIRM) -> 'true';
resource_exists(_AccountId, ?PASSWORD_CHANGE) -> 'true';
resource_exists(_AccountId, ?LOGOUT) -> 'true';
resource_exists(_AccountId, _Path) -> 'false'.

%% /api/v1/account
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  authenticate_with_method(Context, cb_context:req_verb(Context)).

authenticate_with_method(_Context, ?HTTP_PUT) ->
  true ;

authenticate_with_method(Context, ?HTTP_GET) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/account/accountid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _Path) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/account/accountid/path
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(_Context, _AccountId, ?CONFIRM) ->
  true ;

authenticate(Context, _AccountId, ?PASSWORD_CHANGE) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(Context, _AccountId, ?LOGOUT) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(_Context, _AccountId, _Path) -> false. 

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) ->
  authorize_verb(Context, cb_context:req_verb(Context)).

authorize(Context, ?PATH_CREATE = Path) ->
    authorize_util:authorize(?MODULE, Context, {permission, ?PERMISSION_CREATE_USER}); 

authorize(Context, ?PATH_PROFILE = Path) -> true;

authorize(_Context, _Id) ->
    true. 

authorize(_Context, _Id, _Path) ->
    true.

authorize_verb(Context, ?HTTP_PUT) -> true;
authorize_verb(Context, _) -> authorize_util:authorize(?MODULE, Context).

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/account
validate(Context) ->
  ReqJson = cb_context:req_json(Context),
  lager:debug("validate ReqJson: ~p~n",[ReqJson]),
  validate_accounts(Context, cb_context:req_verb(Context)).


%% Validate resource : /api/v1/account/accountid
validate(Context, AccountId) ->
  validate_accounts(Context, AccountId, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/account/accountid/path
validate(Context, AccountId, Path) ->
  validate_accounts(Context, AccountId, Path, cb_context:req_verb(Context)).

-spec validate_accounts(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_accounts(cb_context:context(), path_token(), http_method()) -> cb_context:context().
-spec validate_accounts(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().

%% PUT /api/v1/account
validate_accounts(Context, ?HTTP_PUT = Verb) ->
  
  validate_request(Context, Verb);

%% GET /api/v1/account
validate_accounts(Context, ?HTTP_GET = Verb) ->
  validate_request(Context, Verb).

%% POST /api/v1/account/accountid
validate_accounts(Context, Path, ?HTTP_PUT = Verb) ->
  validate_request(Path, Context, Verb);

validate_accounts(Context, AccountId, ?HTTP_POST = Verb) ->
  validate_request(AccountId, Context, Verb);

%% GET /api/v1/account/accountid
validate_accounts(Context, AccountId, ?HTTP_GET = Verb) ->
  validate_request(AccountId, Context, Verb).

%% POST /api/v1/account/accountid/path
validate_accounts(Context, AccountId, Path, ?HTTP_POST = Verb) ->
  validate_request(AccountId, Context, Path, Verb);

validate_accounts(Context, _AccountId, _Path, _Verb) ->
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
  lager:debug("register account ReqJson: ReqJson: ~p~n",[ReqJson]),
  Debug =  wh_json:get_value(<<"debug">>, ReqJson,<<"false">>),
  Email  = wh_json:get_value(<<"email">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  Uuid = zt_util:get_uuid(),
  AccountId = <<"account", Uuid/binary>>,
  UserId = <<"user", Uuid/binary>>,
  ConfirmCode = zt_util:create_random_number(),
  AccountInfo = #{
    id => AccountId,
    name => <<FirstName/binary," ",LastName/binary>>,
    created_by => UserId,
    created_time => zt_datetime:get_now()
  },
  account_db:save(AccountInfo),
  UserInfo = get_user_info(ReqJson),
  RoleName = <<"admin">>,
  RoleInfo = #{
      related_id => <<"all">>,
      related_type => <<"all">>,
      roles => [RoleName]
  },
  UserDb = maps:merge(UserInfo, #{
                                  id => UserId,
                                  account_id => AccountId,
                                  first_name => FirstName, 
                                  last_name => LastName,
                                  created_by => UserId,	
                                  updated_by => UserId,
                                  confirm_code => ConfirmCode,
                                  roles => [RoleInfo]
                                }),
  user_db:save(UserDb),
Role = 
  #{
    account_id => AccountId,
   created_by => UserId,
   created_time => zt_datetime:get_noew(),
   description => <<"admin">>,
   id => zt_util:get_uuid(),
   name => RoleName,
   permissions => [<<"all">>]
   },
   role_db:save(Role),
lager:debug("UserDb: ~p~n",[UserDb]),
lager:debug("Debug: ~p~n",[Debug]),

  if Debug == <<"true">> ->
       RespData = [{<<"user_id">>, UserId}, {<<"account_id">>, AccountId}, {confirm_code, ConfirmCode}],
       cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData},{fun cb_context:set_resp_status/2, 'success'}]);
     true ->
       spawn(fun() ->  app_util:send_email({create, Context, UserId, Email, FirstName, LastName, ConfirmCode, host, 80}) end),
       RespData = [{<<"user_id">>, UserId},{<<"account_id">>, AccountId}],
       cb_context:setters(Context
                          ,[{fun cb_context:set_resp_data/2, RespData}
                            ,{fun cb_context:set_resp_status/2, 'success'}])
  end.

%% POST api/v1/account/accountid
-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, Id) ->
  ReqJson =  cb_context:req_json(Context),
  %Role = cb_context:role(Context),
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
%% POST api/v1/account/accountid/confirm
%TODO 

%% POST api/v1/account/accountid/change_password
handle_post(Context, _AccountId, _Path) ->
  Context.

permissions() ->
  authorize_util:default_permission(?MODULE).
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
  ValidateFuns = [fun validate_email/2
                  %,fun validate_phone_number/2
                  ,fun validate_password/2
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

%%POST api/v1/account/accountid
validate_request(?PATH_CREATE, Context, ?HTTP_PUT) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),  

  ValidateFuns = [ fun validate_email/2
                  ,fun validate_phone_number/2
                  ,fun validate_password/2
                    ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_AccountId, Context, ?HTTP_POST) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [ fun validate_update_email/2
                   ,fun validate_update_phone_number/2
                   ,fun validate_update_password/2
                   ,fun validate_update_timezone/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_AccountId, Context, _Verb) ->
  Context.


%% POST api/v1/account/accountid/confirm
-spec  validate_request(path_token(), cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_request(_AccountId, Context, ?CONFIRM, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun validate_confirm_code/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%%%%POST api/v1/account/accountid/password_change
validate_request(_AccountId, Context, ?PASSWORD_CHANGE, _Verb) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	

  ValidateFuns = [fun validate_curr_password/2
                  ,fun validate_new_password/2],

  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%% POST api/v1/account/accountid/logout
validate_request(_AccountId, Context, ?LOGOUT, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
  ReqJson =  cb_context:req_json(Context1),
  ValidateFuns = [fun validate_type/2],                        
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_AccountId, Context, _Path, _Verb) ->
  Context.


-spec validate_confirm_code(api_binary(), cb_context:context()) -> cb_context:context().
validate_confirm_code(ReqJson, Context) ->
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
  case ConfirmCode of 
    <<>> ->
      api_util:validate_error(Context, <<"confirm_code">>, <<"required">>, <<"Field 'confirm_code' is required">>);
    _ ->
      Context
  end. 

validate_type(ReqJson, Context) ->
  Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
  if  Type == <<"portal">> ->
        Context;
      true ->
        api_util:validate_error(Context, <<"type">>, <<"required">>, <<"Field 'type' must be portal">>)
  end.

validate_pos_device_id(ReqJson, Context) ->
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
  api_util:check_val(Context, <<"device_id">>, DeviceId).

-spec validate_email(api_binary(), cb_context:context()) -> cb_context:context().
validate_email(ReqJson, Context) ->
  Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	lager:info("Email ~p ~n", [Email]),
  case Email of
    <<>> ->
      api_util:validate_error(Context, <<"email">>, <<"required">>, <<"Field 'email' is required">>);
    _ ->
      case re:run(zt_util:to_str(Email), ?EMAILREGX) of 
        nomatch ->
          api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
        _ -> 
          case is_user_exist(Email) of
            false -> Context;
            Status -> api_util:validate_error(Context, <<"email">>, <<"unique">>, Status)
          end
      end
  end.

-spec validate_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_phone_number(ReqJson, Context) ->
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
  case PhoneNumber of
    <<>> ->
      api_util:validate_error(Context, <<"phone_number">>, <<"required">>, <<"Field 'phone_number' is required">>);
    _  ->
      case re:run(zt_util:to_str(PhoneNumber), ?PHONEREGX) of 
        nomatch ->
          api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"Invalid PhoneNumber">>);
        _ -> 
          Context
      end
  end. 

-spec validate_password(api_binary(), cb_context:context()) -> cb_context:context().
validate_password(ReqJson, Context) ->
  Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
  LenPass = length(zt_util:to_str(Password)),
  case LenPass of
    0 ->
      api_util:validate_error(Context, <<"password">>, <<"required">>, <<"Field 'password' is required">>);
    Val when Val < 8 ->
      api_util:validate_error(Context, <<"password">>, <<"invalid">>, <<"Password must have at least 8 characters">>);
    _ -> 
      Context
  end.

-spec validate_new_password(api_binary(), cb_context:context()) -> cb_context:context().
validate_new_password(ReqJson, Context) ->
  NewPassword = wh_json:get_value(<<"new_password">>, ReqJson, <<>>),
  LenNewPassword = length(zt_util:to_str(NewPassword)),
  case LenNewPassword of
    0 ->
      api_util:validate_error(Context, <<"new_password">>, <<"required">>, <<"Field 'new_password' is required">>);
    Val when Val < 8 ->
      api_util:validate_error(Context, <<"password">>, <<"invalid">>, <<"New Password must have at least 8 characters">>);
    _  ->
      Context
  end.

-spec validate_curr_password(api_binary(), cb_context:context()) -> cb_context:context().
validate_curr_password(ReqJson, Context) ->
  CurrPassword = wh_json:get_value(<<"current_password">>, ReqJson, <<>>),
  api_util:check_val(Context, <<"current_password">>, CurrPassword).


-spec validate_update_email(api_binary(), cb_context:context()) -> cb_context:context().
validate_update_email(ReqJson, Context) ->
  Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
  case Email of 
    <<>> ->
      Context ;
    _ ->
      api_util:validate_error(Context, <<"email">>, <<"forbidden">>, <<"Not Allowed To Update Email">>)
  end. 

-spec validate_update_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_update_phone_number(ReqJson, Context) ->
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
  case PhoneNumber of 
    <<>> ->
      Context ;
    _ ->
      case re:run(zt_util:to_str(PhoneNumber), ?PHONEREGX) of 
        nomatch ->
          api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"Invalid PhoneNumber">>);
        _ -> 
          Context
      end
  end. 

-spec validate_update_password(api_binary(), cb_context:context()) -> cb_context:context().
validate_update_password(ReqJson, Context) ->
  Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
  case Password of 
    <<>> ->
      Context ;
    _ ->
      api_util:validate_error(Context, <<"password">>, <<"forbidden">>, <<"API Not For Update Password">>)
  end. 

validate_update_timezone(ReqJson, Context) ->
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson),
  if TimeZone == undefined -> Context;
     true ->
       app_util:validate_timezone(Context, <<"time_zone">>, TimeZone)
  end.


get_users(QueryJson, Limit, Offset) ->
        user_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).

get_user_info(ReqJson) ->
  Email  = wh_json:get_value(<<"email">>, ReqJson), 
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  Address = wh_json:get_value(<<"address">>, ReqJson, <<>>),
  %Role = wh_json:get_value(<<"role">>, ReqJson, ?USER_ROLE_USER),
  Avatar = wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, <<>>),
  Password = wh_json:get_value(<<"password">>, ReqJson),
  CreatedTime = zt_datetime:get_now(),
  {ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
  {ok, HashPass} = bcrypt:hashpw(Password, Salt),
  
  #{
      email => Email, 
      phone_number => PhoneNumber, 
      first_name => FirstName, 
      last_name => LastName,
      address => Address, 
      password => zt_util:to_bin(HashPass), 
      avatar => Avatar, 
      time_zone => TimeZone, 
      created_time_dt => CreatedTime,
      updated_time_dt => CreatedTime, 
      status => ?INACTIVE, 
      confirm_code_created_time_dt => CreatedTime
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

-spec is_user_exist(binary()) ->  atom().
is_user_exist(Email) ->
	lager:info(<<"Email ~p ~n">>, [Email]),
  case user_db:find_by_conditions([{email_id,Email}],[],1,0) of 
    [#{email := Email, status := Status}] -> Status;
    [] ->
      false;
    _ -> false
    % Error ->
    %   lager:error("User Can't Sign Up. Maybe Database With This Email: ~p; Error: ~p ~n", [Email,Error]),
    %   throw(dberror)
  end. 

get_sub_fields_accounts(User) -> 
  Fields = [password, created_by, created_time_dt, updated_by, updated_time_dt,
            confirm_code, confirm_code_created_time_dt] ,
  NewMap = maps:without(Fields, User),
  Res = maps:to_list(NewMap),
  proplists:substitute_aliases([], Res).