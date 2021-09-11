-module(cb_customers).

-include("crossbar.hrl").
-include("messages.hrl").

-define(PATH_RESEND, <<"resend">>).
-define(PATH_PROFILE, <<"profile">>).
-define(PATH_WALLET, <<"wallet">>).


-export([init/0
         ,allowed_methods/0
         ,allowed_methods/1
         ,allowed_methods/2
         ,allowed_methods/3
         ,validate/1
         ,validate/2
         ,validate/3
         ,validate/4
         ,resource_exists/0
         ,resource_exists/1
         ,resource_exists/2
         ,resource_exists/3
         ,authenticate/1
         ,authenticate/2
         ,authenticate/3
         ,authenticate/4
         ,authorize/1
         ,authorize/2
         ,authorize/3
         ,handle_get/1
         ,handle_get/2
         ,handle_put/1
         ,handle_put/2
         ,handle_post/2
         ,handle_post/3
         ,process_password_grant/3
        ]).

-export([
         get_customer_id/2
         ,issue_token/2
         ,permissions/0
        ]).


init() ->
  _ = crossbar_bindings:bind(<<"*.allowed_methods.customers">>, ?MODULE, 'allowed_methods'),
  _ = crossbar_bindings:bind(<<"*.resource_exists.customers">>, ?MODULE, 'resource_exists'),
  _ = crossbar_bindings:bind(<<"*.validate.customers">>, ?MODULE, 'validate'),
  _ = crossbar_bindings:bind(<<"*.authenticate.customers">>, ?MODULE, 'authenticate'),
  _ = crossbar_bindings:bind(<<"*.authorize.customers">>, ?MODULE, 'authorize'),
  _ = crossbar_bindings:bind(<<"*.to_json.get.customers">>, ?MODULE, 'handle_get'),
  _ = crossbar_bindings:bind(<<"*.execute.post.customers">>, ?MODULE, 'handle_post'),
  _ = crossbar_bindings:bind(<<"*.execute.put.customers">>, ?MODULE, 'handle_put').


% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary(), ne_binary()) -> http_methods().


%% /api/v1/customer
allowed_methods() ->
  [?HTTP_GET, ?HTTP_PUT].

%% /api/v1/customer/customerid
allowed_methods(_CustomerId) ->
  [?HTTP_GET, ?HTTP_POST].

%% /api/v1/customer/customerid/path
allowed_methods(_CustomerId, _Path) ->
  [?HTTP_POST, ?HTTP_DELETE].

%% /api/v1/customer/customerid/path/path
allowed_methods(_CustomerId, _Path1, _Path2) ->
  lager:info("allowed ~n", []),
  [?HTTP_POST].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().
-spec resource_exists(path_token(), ne_binary(), ne_binary()) -> boolean().

%% /api/v1/customer
resource_exists() -> 'true'.

%% /api/v1/customer/customerid
resource_exists(_CustomerId) -> 'true'.

%% /api/v1/customer/path
resource_exists(_CustomerId, ?PATH_CONFIRM) -> 'true';
resource_exists(_CustomerId, ?CLIENTCONFIRM) -> 'true';
resource_exists(_CustomerId, ?PATH_PASSWORD_CHANGE) -> 'true';
resource_exists(_CustomerId, ?PATH_LOGOUT) -> 'true';
resource_exists(_CustomerId, _Path) -> 'false'.

%% /api/v1/customer/path/path
resource_exists(_CustomerId, _Path, _Path) -> 'false'.


%% /api/v1/customer
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  authenticate_with_method(Context, cb_context:req_verb(Context)).

authenticate_with_method(_Context, ?HTTP_PUT) ->
  true ;
authenticate_with_method(Context, ?HTTP_GET) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/customer/customerid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _CustomerId) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

%% /api/v1/customer/customerid/path
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(_Context, _CustomerId, ?PATH_CONFIRM) -> true;
authenticate(_Context, _CustomerId, ?CLIENTCONFIRM) -> true;
authenticate(_Context, _CustomerId, ?PATH_RESEND) -> true;
authenticate(Context, _CustomerId, ?PATH_PASSWORD_CHANGE) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(Context, _CustomerId, ?PATH_LOGOUT) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context);

authenticate(_Context, _CustomerId, _Path) -> false.

-spec authenticate(cb_context:context(), path_token(), path_token(),  path_token()) -> boolean().
authenticate(_Context, _CustomerId, _Path, _Path) -> false.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context) ->
  Verb = cb_context:req_verb(Context),
  authorize_verb(Context, Verb).

authorize_verb(_, ?HTTP_PUT) -> true;
authorize_verb(Context, ?HTTP_GET) ->

  Role = cb_context:role(Context),
  Role == ?USER_ROLE_ADMIN.


authorize(Context, ?PATH_PROFILE) ->
  Role = cb_context:role(Context),
  case Role of
    ?USER_ROLE_CUSTOMER -> true;
    _ -> true
  end;

authorize(Context, ?PATH_WALLET) ->
  authorize(Context, ?PATH_PROFILE);

authorize(_Context, _Id) ->
  lager:debug("authorize: ~p~n",[_Id]),
  true.

authorize(_Context, _Id, _Path) ->
  lager:debug("authorize: ~p~n",[_Id]),
  true.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/customer
validate(Context) ->
  validate_customers(Context, cb_context:req_verb(Context)).


%% Validate resource : /api/v1/customer/customerid
validate(Context, CustomerId) ->
  validate_customers(Context, CustomerId, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/customer/customerid/path
validate(Context, CustomerId, Path) ->
  validate_customers(Context, CustomerId, Path, cb_context:req_verb(Context)).

validate(Context, CustomerId, Path1, Path2) ->
  validate_customers(Context, CustomerId, Path1, Path2, cb_context:req_verb(Context)).


-spec validate_customers(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_customers(cb_context:context(), path_token(), http_method()) -> cb_context:context().
-spec validate_customers(cb_context:context(), path_token(), path_token(), http_method()) -> cb_context:context().

%% PUT /api/v1/customer
validate_customers(Context, ?HTTP_PUT = Verb) ->
  lager:info("customers: validate request: PUT ~n",[]),
  validate_request(Context, Verb);

%% GET /api/v1/customer
validate_customers(Context, ?HTTP_GET = Verb) ->
  lager:info("customers: validate request: GET: ~n",[]),
  %cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]).
  validate_request(Context, Verb).

%% POST /api/v1/customer/customerid
validate_customers(Context, CustomerId, ?HTTP_POST = Verb) ->
  lager:info("customers: validate request: ~p POST ~n",[CustomerId]),
  validate_request(CustomerId, Context, Verb);

%% GET /api/v1/customer/customerid
validate_customers(Context, CustomerId, ?HTTP_GET = Verb) ->
  lager:info("customers: validate request: GET ~n",[]),
  validate_request(CustomerId, Context, Verb).

%% POST /api/v1/customer/customerid/path
validate_customers(Context, CustomerId, Path, ?HTTP_POST = Verb) ->
  lager:info("customers: validate request: Path POST ~n",[]),
  validate_request(CustomerId, Context, Path, Verb).


%% POST /api/v1/customer/customerid/path/path
validate_customers(Context, CustomerId, Path1, Path2, ?HTTP_POST = Verb) ->
  validate_request(CustomerId, Context, Path1, Path2,  Verb).


-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().
%% GET api/v1/customer
handle_get({Req, Context}) ->
  Role = cb_context:role(Context),
  QueryJson = cb_context:query_string(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
  Customers = get_customers(PropQueryJson, Limit, Offset),
  PropCustomers = lists:map(fun(Customer) ->
               customer_util:get_sub_fields_customers(Customer)
  end, Customers),
  {Req, cb_context:setters(Context,[{fun cb_context:set_resp_data/2, PropCustomers}
                                         ,{fun cb_context:set_resp_status/2, 'success'}])}.

handle_get({Req, Context}, ?PATH_PROFILE) ->
  CustomerId = cb_context:customer_id(Context),
  lager:debug("Get profile CustomerId: ~p~n",[CustomerId]),
  CustomerInfo = customer_db:find(CustomerId),
  PropCustomer = customer_util:get_sub_fields_customers(CustomerInfo),
  Groups = group_db:find_by_conditions([{<<"members#id">>,CustomerId}],[],10,0),
  FilteredGroups = 
  lists:map(fun(#{members := Members} = GroupInfo) -> 
    
    MemberInfo = maps:with([id, type, name], GroupInfo),
    maps:merge(MemberInfo, #{
      role => find_role(Members, CustomerId)
    })
  end,Groups),
  CustomerMap = maps:from_list(PropCustomer),
  lager:debug("PropCustomer: ~p~n",[PropCustomer]),
  PropCustomer2 = maps:merge(CustomerMap, #{
    groups => FilteredGroups
  }),
  {Req, cb_context:setters(Context
                           ,[{fun cb_context:set_resp_data/2, PropCustomer2}
                             ,{fun cb_context:set_resp_status/2, 'success'}
                            ])};



%% GET api/v1/customer/customerid
handle_get({Req, Context}, CustomerId) ->
  RequesterId = cb_context:customer_id(Context),
  Role = cb_context:role(Context),
  lager:debug("Get customer by CustomerId: ~p~n",[RequesterId]),
  if Role == ?USER_ROLE_CUSTOMER andalso RequesterId == CustomerId ->
       Customer = cb_context:account_doc(Context),
       PropCustomer = customer_util:get_sub_fields_customers(Customer),
       {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, PropCustomer}
                                  ,{fun cb_context:set_resp_status/2, 'success'}
                                 ])};
     true ->
       {Req, cb_context:setters(Context,
                                [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                                 {fun cb_context:set_resp_status/2, <<"error">>},
                                 {fun cb_context:set_resp_error_code/2, 403}
                                ])}
  end.

find_role([], Id) -> <<>>;

find_role([MemberInfo|OtherMembers], Id) -> 
      case MemberInfo of 
      #{
        <<"id">> := Id, 
        <<"role">> := Role 
      } ->  Role;
      _ -> find_role(OtherMembers, Id)
    end.
%% PUT api/v1/customer
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
  ReqJson =  cb_context:req_json(Context),
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
  ReferralCode = wh_json:get_value(<<"referral_code">>, ReqJson, <<>>),
  SendSms = wh_json:get_value(<<"debug">>, ReqJson, <<"true">>),
  case maybe_create_customer(PhoneNumber,ReqJson,ReferralCode,<<>>) of
    {error, ErrorMsg} -> 
        cb_context:setters(Context,
                  [{fun cb_context:set_resp_error_msg/2, ErrorMsg},
                  {fun cb_context:set_resp_status/2, <<"error">>},
                  {fun cb_context:set_resp_error_code/2, 400}
                ]);
    {CustomerId, ConfirmCode, ReferrerId,IsNewCustomer} -> 
          maybe_send_sms(SendSms,PhoneNumber,ConfirmCode),
          Resp =[
            {<<"customer_id">>, CustomerId},
            {<<"confirm_code">>, ConfirmCode},
            {<<"is_new_customer">>, IsNewCustomer}
        ],
        cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, Resp}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                              ])
   end.
                
      
handle_referral(<<>>,_) -> ok;

handle_referral(ReferralId,CustomerId) ->
  Data = #{
    referree_id => CustomerId,
    referrer_id => ReferralId
  },
  promotion_handlers:handler_rules(<<"referral">>,Data).

maybe_create_customer(PhoneNumber,ReqJson, <<>>, ReferrerId) -> 
   get_customer_id(ReqJson, PhoneNumber,ReferrerId);

maybe_create_customer(PhoneNumber,ReqJson,ReferralCode, _) -> 
  case customer_db:find_by_referral_code(ReferralCode) of
    [#{id := ReferrerId} = ReferrerDb] ->
      maybe_create_customer(PhoneNumber,ReqJson, <<>>, ReferrerId);
    _ ->
      {error, <<"Referral code is invalid">>}
  end.

maybe_send_sms(<<"true">>,_,_) -> ok;
maybe_send_sms(_,PhoneNumber,ConfirmCode) -> 

  zt_r2r_util:send_sms(?DEFAULT_SENDER_PHONE_NUMBER,PhoneNumber,list_to_binary(?EN_CUSTOMER_CONFIRM_CODE ++ ConfirmCode)).

%% PUT api/v1/customer/customerid
-spec handle_put(cb_context:context(), path_token()) -> cb_context:context().
handle_put(Context, _CustomerId) ->
  ?MODULE:handle_put(Context).

%% POST api/v1/customer/customerid
-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().

handle_post(Context, ?PATH_PROFILE) ->
  CustomerId = cb_context:customer_id(Context),
  handle_post(Context, CustomerId);

handle_post(Context, CustomerId) ->
  AccountIdOwnerToken = cb_context:customer_id(Context),
  RoleAccountOwnerToken = cb_context:role(Context),
  if AccountIdOwnerToken == CustomerId ->
       ReqJson =  cb_context:req_json(Context),
       %Customer = cb_context:account_doc(Context),
       Customer = customer_db:find(CustomerId),
       CustomerInfo = update_customer(ReqJson, Customer),
       NewCustomer =  maps:merge(CustomerInfo, #{updated_by_id =>AccountIdOwnerToken}),
       customer_db:save(NewCustomer),
       RespData = customer_util:get_sub_fields_customers(NewCustomer),
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
  end.

%% POST api/v1/customers/customerid/confirm
-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, CustomerId, ?PATH_CONFIRM) ->
  lager:debug("handle_post confirm customerid: ~p~n",[CustomerId]),
  ReqJson =  cb_context:req_json(Context),
  CurrentTimeToSecond = zt_util:timestamp_second(),
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
  CustomerInfo = cb_context:account_doc(Context),
  ServerConfirmCode = maps:get(confirm_code, CustomerInfo, <<>>),
  ConfirmCodeCreatedTime = maps:get(confirm_code_created_time_dt, CustomerInfo, <<>>),
  ConfirmCodeCreatedTimeToSecond = zt_util:datetime_binary_to_second(ConfirmCodeCreatedTime),
  lager:debug("emnvn CustomerId: ~p ; ConfirmCode: ~p; ServerConfirmCode: ~p CurrentSecond: ~p Confirmcode created time: ~p ~n",[CustomerId, ConfirmCode, ServerConfirmCode,CurrentTimeToSecond,ConfirmCodeCreatedTimeToSecond]),
  if CurrentTimeToSecond - ConfirmCodeCreatedTimeToSecond > ?DATESECOND ->
       cb_context:setters(Context,
                          [{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
                           {fun cb_context:set_resp_status/2, <<"error">>},
                           {fun cb_context:set_resp_error_code/2, 400}
                          ]);
     true  ->
       if ConfirmCode /= <<>> andalso ConfirmCode =:= ServerConfirmCode ->
            Scope    =  wh_json:get_value(<<"scope">>, ReqJson, ?USER_ROLE_CUSTOMER),
            Auth     = oauth2:authorize_password({CustomerId, ConfirmCode}, <<>>, Scope, [{scope, Scope}]),
            NewContext = issue_token(Auth, Context),

           	%---Device---%
						ReqDeviceInfo = wh_json:get_value(<<"device_info">>, ReqJson, []),
						ReqDeviceId = proplists:get_value(<<"device_id">>, ReqDeviceInfo, <<>>),
						OwnerInfo = [{<<"account_id">>, CustomerId},{<<"account_scope">>, Scope}],
						DeviceInfo = lists:merge(ReqDeviceInfo, OwnerInfo),
						DeviceId =	cb_device:add_device(ReqDeviceId, {DeviceInfo}),

            ResponseData = cb_context:resp_data(NewContext),
            NewResponseData =  [{<<"device_id">>,  DeviceId}|ResponseData],
            #{
              first_name := FirstName,
              last_name := LastName
            } = customer_db:find(CustomerId),
            cb_context:setters(NewContext ,[
              {fun cb_context:set_resp_data/2, NewResponseData}
            ]);
          true ->
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 400}
                               ])
       end
  end;

handle_post(Context, CustomerId, ?PATH_RESEND) ->
  ReqJson =  cb_context:req_json(Context),
  
  IsDebug = wh_json:get_value(<<"debug">>, ReqJson, <<"true">>),
  case customer_db:find(CustomerId) of
    #{phone_number := PhoneNumber} = CustomerInfo ->
      lager:debug("CUSTOMER_EXIST: ~p ~n",[CustomerInfo]),
      ConfirmCode = create_confirm_code_by_phone(PhoneNumber),
    ConfirmCodeCreatedTime = zt_datetime:get_now(),
      customer_util:update_customer_info([
                                          {confirm_code , ConfirmCode},
                                          {confirm_code_created_time_dt , ConfirmCodeCreatedTime}
                                         ],CustomerInfo),
      RespData = case IsDebug of
                   <<"true">> -> [{<<"id">>, CustomerId},{<<"confirm_code">>, ConfirmCode}];
                   _ ->
                     zt_r2r_util:send_sms(?DEFAULT_SENDER_PHONE_NUMBER,PhoneNumber,list_to_binary(?EN_CUSTOMER_CONFIRM_CODE ++ ConfirmCode)),
                     [{<<"customer_id">>, CustomerId}]
                 end,
      cb_context:setters(Context
                         ,[{fun cb_context:set_resp_data/2, RespData}
                           ,{fun cb_context:set_resp_status/2, 'success'}
                          ]);

    _ -> cb_context:setters(Context,
                            [{fun cb_context:set_resp_error_msg/2, <<"Not Found Customer">>},
                             {fun cb_context:set_resp_status/2, <<"error">>},
                             {fun cb_context:set_resp_error_code/2, 404}
                            ])

  end;

%% POST api/v1/customer/customerid/change_password
handle_post(Context, CustomerId, ?PATH_PASSWORD_CHANGE) ->
  RequesterId = cb_context:customer_id(Context),
  if
    RequesterId == CustomerId ->
      ReqJson =  cb_context:req_json(Context),
      Customer = cb_context:account_doc(Context),
      CurrPass  = wh_json:get_value(<<"current_password">>, ReqJson, <<>>),
      NewPass = wh_json:get_value(<<"new_password">>, ReqJson, <<>>),
      CurrPassHashServer = maps:get(password, Customer, <<>>),
      lager:debug("CurrPass: NewPass: ~p ~n",[{CurrPass, NewPass, CurrPassHashServer}]),
      PassHash = zt_util:to_str(CurrPassHashServer),
      {ok, ProvidedHash} = bcrypt:hashpw(CurrPass, PassHash),
      lager:debug("NewCustomer;  CurrPass; CurrPassServer: ~p ~n",[{NewPass, CurrPass, CurrPassHashServer, ProvidedHash}]),
      if
        CurrPass /= <<>> andalso ProvidedHash == PassHash ->
          UpdatedTime = zt_datetime:get_now(),
          UpdatedBy = RequesterId,
          {ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
          {ok, NewPassHash} = bcrypt:hashpw(NewPass, Salt),
          api_doc:update_user_info( [{password, NewPassHash},
                                     {updated_time_dt, UpdatedTime},
                                     {updated_by, UpdatedBy}], Customer),
          api_doc:del_tokens_of_user(CustomerId),
          RespData = [{<<"customer_id">>, CustomerId}],
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


%% POST api/v1/customer/customer_id/logout
handle_post(Context, CustomerId, ?PATH_LOGOUT) ->
  Role = cb_context:role(Context),
  ReqJson =  cb_context:req_json(Context),
  CustomerIdDb = cb_context:customer_id(Context),
  Token = cb_context:auth_token(Context),
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
  if  Role == ?USER_ROLE_ADMIN;
    Role == ?USER_ROLE_CUSTOMER andalso CustomerId == CustomerIdDb ->
        case device_db:find(DeviceId) of
          #{customer_id := CustomerIdDb, status := ?ACTIVE} = Device ->
            lager:info("Device ~p ~n", [Device]),
            DeviceInfo = maps:merge(Device, #{status => ?INACTIVE}),
            device_db:save(DeviceInfo),
            access_token_mnesia_db:del_by_token(Token),
            refresh_token_db:del_by_token(Token),
            RespData = [{token, Token}, {device_id, DeviceId}],
            cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData}
                                         ,{fun cb_context:set_resp_status/2, 'success'}]);

          #{customer_id := CustomerIdDb, status := ?INACTIVE}  ->
            access_token_mnesia_db:del_by_token(Token),
            RespData = [{token, Token}, {device_id, DeviceId}],
            cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData}
                                         ,{fun cb_context:set_resp_status/2, 'success'}]);
          _ ->
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_error_msg/2, <<"Device Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])
        end;
      true ->
        cb_context:setters(Context,
                           [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
                            {fun cb_context:set_resp_status/2, <<"error">>},
                            {fun cb_context:set_resp_error_code/2, 403}])
  end;



handle_post(Context, CustomerId, ?CLIENTCONFIRM) ->
  lager:debug("handle_post client confirm customerid: ~p~n",[CustomerId]),
  ReqJson =  cb_context:req_json(Context),
  CurrentTimeToSecond = zt_util:timestamp_second(),
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
  case customer_db:find(CustomerId) of
    #{confirm_code := ServerConfirmCode, confirm_code_created_time_dt := ConfirmCodeCreatedTime} ->
      ConfirmCodeCreatedTimeToSecond = zt_util:datetime_binary_to_second(ConfirmCodeCreatedTime),
      if CurrentTimeToSecond - ConfirmCodeCreatedTimeToSecond > ?DATESECOND ->
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 400}
                              ]);
         true  ->
           if ConfirmCode /= <<>> andalso ConfirmCode =:= ServerConfirmCode ->
                Scope    =  wh_json:get_value(<<"scope">>, ReqJson, ?USER_ROLE_CUSTOMER),
                Auth     = oauth2:authorize_password({CustomerId, ConfirmCode}, <<>>, Scope, [{scope, Scope}]),
                NewContext = issue_token(Auth, Context),
                ClientId = wh_json:get_value(<<"client_id">>, ReqJson, <<>>),
                OsType = wh_json:get_value(<<"os_type">>, ReqJson, <<>>),
                case is_client_id_existed(ClientId) of
                  true ->
                    cb_context:setters(Context,
                                       [{fun cb_context:set_resp_data/2, [{client_id, ClientId}]},
                                        {fun cb_context:set_resp_error_msg/2, <<"ClientId Existed">>},
                                        {fun cb_context:set_resp_status/2, <<"error">>},
                                        {fun cb_context:set_resp_error_code/2, 400}])
                    ;
                  _ ->
                    cb_device:add_client(CustomerId, ClientId, OsType),
                    ResponseData = cb_context:resp_data(NewContext),
                    NewResponseData =  [{<<"client_id">>,  ClientId}|ResponseData],
                    cb_context:setters(NewContext ,[{fun cb_context:set_resp_data/2, NewResponseData}])
                end;
              true ->
                cb_context:setters(Context,
                                   [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
                                    {fun cb_context:set_resp_status/2, <<"error">>},
                                    {fun cb_context:set_resp_error_code/2, 400}
                                   ])
           end
      end;
    _ ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"Not Found Customer">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 404}
                         ])


  end;

%% POST api/v1/customer/customerid/path
handle_post(Context, _CustomerId, _Path) ->
  Context.

%handle_post(Context, _CustomerId, _Path, _Path) ->
%  Context.

is_client_id_existed(ClientId) ->
  case device_db:find_by_client_id(ClientId) of
    [] ->
      false;
    _ ->
      true
  end.

permissions() ->
  authorize_util:default_permission(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_customer_info([]) -> #{};
get_customer_info(ReqJson) ->
  Addresses = wh_json:get_value(<<"addresses">>, ReqJson, []),
  DeliveryAddresses = wh_json:get_value(<<"delivery_addresses">>, ReqJson, []),
  Avatar  = wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, <<>>),
  ReferralCode = customer_util:get_referral_code(),
  #{
    first_name => FirstName,
    last_name => LastName,
    avatar => Avatar,
    time_zone => TimeZone,
    addresses => Addresses,
    delivery_addresses => DeliveryAddresses,
    referral_code => ReferralCode
   }.

  get_customer_id(ReqJson, PhoneNumber) -> 
    {CustomerId, ConfirmCode,_,_} = get_customer_id(ReqJson, PhoneNumber, <<>>),
    {CustomerId, ConfirmCode}.

create_confirm_code_by_phone(PhoneNumber) -> 
case binary:match(PhoneNumber,<<"65303556">>) of
  nomatch -> 
    zt_util:create_random_number();
  _ -> <<"1111">>
end.

get_customer_id(ReqJson, PhoneNumber, ReferrerId) ->

  ConfirmCode = create_confirm_code_by_phone(PhoneNumber),
    

  ConfirmCodeCreatedTime = zt_datetime:get_now(),

  case customer_db:find_by_phone_number(PhoneNumber) of
    [#{phone_number := PhoneNumber}] = [CustomerInfo] ->
      customer_util:update_customer_info([
                                {confirm_code , ConfirmCode},
                                {confirm_code_created_time_dt , ConfirmCodeCreatedTime}
                            ],CustomerInfo),
      lager:info("Get customerid. Status: customer existed. ~n ", []),
      Id = customer_util:get_id(CustomerInfo),
      {Id, ConfirmCode,<<>>,false};
    [] ->
      CustomerInfo = get_customer_info(ReqJson),
      Uuid = zt_util:get_uuid(),
      CustomerId = <<"customer", Uuid/binary>>,
      CustomerDb =  maps:merge(CustomerInfo, #{id => CustomerId,
                                               phone_number => PhoneNumber,
                                               confirm_code_created_time_dt => ConfirmCodeCreatedTime,
                                               confirm_code => ConfirmCode,
                                               status => ?ACTIVE,
                                               created_by => CustomerId,
                                               updated_by => CustomerId,
                                               referred_by => ReferrerId,
                                               created_time_dt => ConfirmCodeCreatedTime,
                                               updated_time_dt => ConfirmCodeCreatedTime}),
      customer_db:save(CustomerDb),
      {CustomerId, ConfirmCode, ReferrerId,true};

    ErrData ->
      lager:error("Get customerid. Status: Data is invalid. Detail ErrData: ~p ~n",[ErrData]),
      throw(dberror)
  end.

%% @doc:
%% Condition to validate referral
%% Case 1: ReferredBy == <<>> -> return ReferrerId
map_referrer_id(<<>>, ReferrerId) ->
  ReferrerId;
%% Case 2: Existed ReferredBy -> return ReferredBy
map_referrer_id(ReferredBy, _ReferrerId) ->
  <<>>.

-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().
validate_request(Context, ?HTTP_PUT) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),

  ValidateFuns = [fun validate_phone_number/2,
                  fun validate_referral_code/2],
  lists:foldl(fun(F, C) ->
                  F('undefined', C)
              end, Context1,  ValidateFuns);


%%GET api/v1/customer
validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, _Verb) ->
  Context.


%%GET api/v1/customer/customerid
-spec  validate_request(path_token(), cb_context:context(), http_method()) -> cb_context:context().

validate_request(?PATH_PROFILE, Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);

 
 validate_request(?PATH_WALLET, Context, ?HTTP_GET) ->
                      cb_context:setters(Context
                                         ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(CustomerId, Context, ?HTTP_GET) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),

  ValidateFuns = [fun validate_exist_customer/2],
  lists:foldl(fun(F, C) ->
                  F(CustomerId, C)
              end, Context1,  ValidateFuns);


%%POST api/v1/customer/customerid
validate_request(?PATH_PROFILE, Context, ?HTTP_POST) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(CustomerId, Context, ?HTTP_POST) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),
  ValidateFuns = [ fun validate_exist_customer/2
                   ,fun validate_email/2
                   ,fun validate_update_phone_number/2],
  lists:foldl(fun(F, C) ->
                  F(CustomerId, C)
              end, Context1,  ValidateFuns);

validate_request(_CustomerId, Context, _Verb) ->
  Context.


%% POST api/v1/customer/customerid/confirm
-spec  validate_request(path_token(), cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_request(CustomerId, Context, ?PATH_CONFIRM, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),
  ValidateFuns = [fun validate_exist_customer/2
                  ,fun validate_confirm_code/2
                  %,fun validate_device/2
                 ],
  lists:foldl(fun(F, C) ->
                  F(CustomerId, C)
              end, Context1,  ValidateFuns);

validate_request(_CustomerId, Context, ?PATH_RESEND, _Verb) ->
  cb_context:setters(Context ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_CustomerId, Context, ?CLIENTCONFIRM, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),
  ReqJson =  cb_context:req_json(Context1),
  ValidateFuns = [ fun validate_client_id/2
                   , fun validate_confirm_code/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

%% POST api/v1/customer/customerid/logout
validate_request(_CustomerId, Context, ?PATH_LOGOUT, _Verb) ->
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),
  ReqJson =  cb_context:req_json(Context1),
  ValidateFuns = [fun validate_device_id/2],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_CustomerId, Context, _Path, _Verb) ->
  Context.

%% POST api/v1/customer/customerid/bot/confirm

validate_request(_CustomerId, Context, _Path1, _Path2, _Verb) ->
  Context.




-spec validate_exist_customer(api_binary(), cb_context:context()) -> cb_context:context().
validate_exist_customer(CustomerId, Context) ->
  Customer = customer_util:get_customer(CustomerId),
  case maps:size(Customer) of
    Val when Val /= 0 ->
      cb_context:setters(Context
                         ,[{fun cb_context:set_account_doc/2, Customer}]);
    _ ->
      api_util:validate_error(Context, <<"customer">>, <<"invalid">>, <<"Customer Not found">>)
  end.

-spec validate_confirm_code(api_binary(), cb_context:context()) -> cb_context:context().
validate_confirm_code(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
  case ConfirmCode of
    <<>> ->
      api_util:validate_error(Context, <<"confirm_code">>, <<"required">>, <<"Field 'confirm_code' is required">>);
    _ ->
      Context
  end.

validate_device_id(ReqJson, Context) ->
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
  lager:info("DeviceId ~p ~n", [DeviceId]),
  api_util:check_val(Context, <<"device_id">>, DeviceId).

validate_client_id(ReqJson, Context) ->
  DeviceId = wh_json:get_value(<<"client_id">>, ReqJson, <<>>),
  lager:info("DeviceId ~p ~n", [DeviceId]),
  api_util:check_val(Context, <<"client_id">>, DeviceId).

validate_device(_CustomerId, Context) ->
  ReqJson =  cb_context:req_json(Context),
  DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),

  Funs = [fun validate_app_id/3
          , fun validate_push_id/3
          , fun validate_os_type/3
          , fun validate_env/3],
  lists:foldl(fun(F, C) ->
                  F(DeviceId, ReqJson, C)  end, Context, Funs).
validate_app_id(DeviceId, ReqJson,  Context) ->
  case DeviceId of
    <<>> -> AppId = wh_json:get_value(<<"app_id">>, ReqJson, <<>>),
            api_util:check_val(Context, <<"app_id">>, AppId);
    _ -> Context
  end.
validate_push_id(DeviceId, ReqJson, Context) ->
  case DeviceId of
    <<>> ->
      PushId = wh_json:get_value(<<"push_id">>, ReqJson, <<>>),
      api_util:check_val(Context, <<"push_id">>, PushId);
    _ -> Context
  end.
validate_os_type(DeviceId, ReqJson, Context) ->
  case DeviceId of
    <<>> ->  OsType = wh_json:get_value(<<"os_type">>, ReqJson, <<>>),
             case lists:member(OsType, ?OS_TYPE) of
               true -> Context;
               _ -> api_util:validate_error(Context, <<"os_type">>, <<"forbidden">>, <<"OsType must be ios/android">>)
             end;
    _ -> Context
  end.

validate_env(DeviceId, ReqJson, Context) ->
  case DeviceId of
    <<>> -> Env = wh_json:get_value(<<"env">>, ReqJson, <<>>),
            case lists:member(Env, ?ENV) of
              true ->  Context;
              _ -> api_util:validate_error(Context, <<"env">>, <<"forbidden">>, <<"Env must be dev/prod">>)
            end;
    _ -> Context
  end.




-spec validate_email(api_binary(), cb_context:context()) -> cb_context:context().
validate_email(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
  case Email of
    <<>> ->
      Context;
    _ ->
      case re:run(zt_util:to_str(Email), ?EMAILREGX) of
        nomatch ->
          api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
        _ ->
          Context
      end
  end.

-spec validate_company_name(api_binary(), cb_context:context()) -> cb_context:context().
validate_company_name(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  CompanyName = wh_json:get_value(<<"company_name">>, ReqJson, <<>>),
  case CompanyName of
    <<>> ->
      api_util:validate_error(Context, <<"company_name">>, <<"required">>, <<"Field 'company_name' is required">>);
    _  ->
      Context
  end.

-spec validate_company_address(api_binary(), cb_context:context()) -> cb_context:context().
validate_company_address(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  CompanyAddress = wh_json:get_value(<<"company_address">>, ReqJson, <<>>),
  case CompanyAddress of
    <<>> ->
      api_util:validate_error(Context, <<"company_address">>, <<"required">>, <<"Field 'company_address' is required">>);
    _  ->
      Context
  end.

-spec validate_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_phone_number(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
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

validate_referral_code(_, Context) ->
  ReqJson = cb_context:req_json(Context),
  Value = wh_json:get_value(<<"referral_code">>, ReqJson),
  case Value of
    <<>> ->
      api_util:validate_error(Context, <<"referral_code">>, <<"invalid">>, <<"Invalid `referral_code`">>);
    _ ->
      %% @doc: referral_code = undefined|referral_code /= <<>>
      Context
  end.



-spec validate_update_email(api_binary(), cb_context:context()) -> cb_context:context().
validate_update_email(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
  case Email of
    <<>> ->
      Context ;
    _ ->
      api_util:validate_error(Context, <<"email">>, <<"forbidden">>, <<"Not Allowed To Update Email">>)
  end.

-spec validate_update_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_update_phone_number(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
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
validate_update_password(_CustomerId, Context) ->
  ReqJson = cb_context:req_json(Context),
  Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
  case Password of
    <<>> ->
      Context ;
    _ ->
      api_util:validate_error(Context, <<"password">>, <<"forbidden">>, <<"API Not For Update Password">>)
  end.

-spec is_customer_exist(ne_binary()) ->  boolean().
is_customer_exist(Email) ->
  customer_util:is_email_exist(Email) .


-spec is_phonenumber_exist(ne_binary()) ->  boolean().
is_phonenumber_exist(PhoneNumber) ->
  customer_util:is_phonenumber_exist(PhoneNumber) .


process_password_grant(Context, CustomerId, PhoneNumber) ->
  Scope = <<"yourbase">>,
  Auth     = oauth2:authorize_password({CustomerId, PhoneNumber}, <<"yourbase">>, []),
  issue_token(Auth, Context).


issue_token({ok, {Ctx,Auth}}, Context) ->
  emit_response(oauth2:issue_token_and_refresh(Auth, Ctx), Context);

issue_token(Error, Context) ->
  emit_response(Error, Context).

emit_response(AuthResult, Context) ->
  {Status, Code, Resp, Msg} =
  case AuthResult of
    {error, Reason} ->
      {'error', 400, [], to_binary(Reason)};

    {ok,{Ctx,Response}} ->
      Res = oauth2_response:to_proplist(Response) ++ Ctx,
      {'success', 200, Res, <<>>}
  end,
  AuthToken = proplists:get_value(<<"access_token">>, Resp),
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_error_msg/2, Msg}
                       ,{fun cb_context:set_resp_status/2, Status}
                       ,{fun cb_context:set_resp_error_code/2, Code}
                       ,{fun cb_context:set_resp_data/2, proplists:delete(<<"access_token">>, Resp)}
                       ,{fun cb_context:set_auth_token/2, AuthToken}
                      ]).

to_binary(Atom) when is_atom(Atom) ->
  list_to_binary(atom_to_list(Atom)).


get_customers(QueryJson, Limit, Offset) ->
  customer_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).



update_customer(ReqJson, Customer) ->
  lager:debug("update_customer ReqJson: ~p~n",[ReqJson]),
  FirstName = wh_json:get_value(<<"first_name">>, ReqJson, maps:get(first_name, Customer, <<>>)),
  LastName = wh_json:get_value(<<"last_name">>, ReqJson, maps:get(last_name, Customer, <<>>)),
  Gender = wh_json:get_value(<<"gender">>, ReqJson, maps:get(gender, Customer, <<>>)),
  Addresses = wh_json:get_value(<<"addresses">>, ReqJson, maps:get(addresses, Customer, [])),
  DeliveryAddresses = wh_json:get_value(<<"delivery_addresses">>, ReqJson, maps:get(delivery_addresses, Customer, [])),
  Avatar = wh_json:get_value(<<"avatar">>, ReqJson, maps:get(avatar, Customer, <<>>)),
  TimeZone = wh_json:get_value(<<"time_zone">>, ReqJson, maps:get(time_zone, Customer, <<>>)),
  Email = wh_json:get_value(<<"email">>, ReqJson, maps:get(email, Customer, <<>>)),
  UpdateTime =  zt_datetime:get_now(),
  maps:merge(Customer, #{
    first_name => FirstName, 
    last_name => LastName, 
    gender => Gender,
    email => Email, 
    addresses => Addresses,
    delivery_addresses => DeliveryAddresses, 
    avatar => Avatar, 
    time_zone => TimeZone,
    updated_time_dt => UpdateTime
  }).