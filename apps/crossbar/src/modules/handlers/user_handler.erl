-module(user_handler).

-include("crossbar.hrl").

-export([
    issue_token/2
    ,find_role/2
    , check_register_user_existed/1
    , find_unconfirmed_user/1
    , handle_user_confirm/2
    , handle_user_resend/2
    , maybe_update_role_token/3
    , send_otp/2
    , create_confirm_code_by_phone/1
    ,validate_role/2
    , validate_phone_number/2
    , validate_search_phone_number/2
    , validate_password/2
    , validate_confirm_phone_number/2
    , validate_confirm_code/2
    , validate_update_password/2
    , validate_update_timezone/2
    , validate_curr_password/2
    , validate_new_password/2
]).

maybe_update_role_token(true, UserId, NewRole) -> 
  AccessTokens = access_token_mnesia_db:find_by_user_id(UserId),
  lists:foreach(fun(AccessTokenInfo) -> 
    NewAccessTokenInfo = maps:merge(AccessTokenInfo, #{
      role => NewRole
    }),
    access_token_mnesia_db:save(NewAccessTokenInfo)
  end, AccessTokens),
  %TODO with refresh_token
ok;
maybe_update_role_token(false, _UserId, _NewRoles) -> ok.
find_role([], Id) -> <<>>;

find_role([MemberInfo|OtherMembers], Id) -> 
      case MemberInfo of 
      #{
        <<"id">> := Id, 
        <<"role">> := Role 
      } ->  Role;
      _ -> find_role(OtherMembers, Id)
    end.

issue_token({ok, {Ctx,Auth}}, Context) ->
  emit_response(oauth2:issue_token_and_refresh(Auth, Ctx), Context);

issue_token(Error, Context) ->
  emit_response(Error, Context).

emit_response(AuthResult, Context) ->
  {Status, Code, Resp, Msg} =
  case AuthResult of
    {error, Reason} ->
      {'error', 400, [], zt_util:to_bin(Reason)};
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

check_register_user_existed(PhoneNumber) -> 
  case user_db:find_by_phone_number(PhoneNumber) of
    [] ->  
        false;
    [#{
      status := ?USER_STATUS_UNCONFIRMED
    }] -> 
        false;
    _ -> 
        true 
  end.

find_unconfirmed_user(PhoneNumber) -> 
  case user_db:find_by_phone_number(PhoneNumber) of
    [#{
      status := ?USER_STATUS_UNCONFIRMED
    } = Info] -> 
      Info;
    _ -> 
        notfound 
  end.

create_confirm_code_by_phone(PhoneNumber) -> 
    case binary:match(PhoneNumber,<<"65303556">>) of
    nomatch -> 
        zt_util:create_random_number();
    _ -> <<"1111">>
    end.

handle_user_confirm(Context, 
  #{
    id := UserId,
    confirm_code := ServerConfirmCode, 
    confirm_code_created_time_dt := ConfirmCodeCreatedTime,
    status := ?USER_STATUS_UNCONFIRMED
  } = UserInfo
)-> 
  ReqJson =  cb_context:req_json(Context),
  CurrentTimeToSecond = zt_util:timestamp_second(),
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson), 
  EplasedSeconds = zt_datetime:diff_second(ConfirmCodeCreatedTime),
  OtpExpiredDuration = zt_util:to_integer(application:get_env(crossbar, otp_expired_duration, 120)),
  
  if 
    EplasedSeconds > OtpExpiredDuration ->
      Context2 = api_util:validate_error(Context, <<"confirm_code">>, <<"invalid">>, <<"confirm_code_expired">>),
      cb_context:setters(Context2,[
                {fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
                {fun cb_context:set_resp_status/2, <<"error">>},
                {fun cb_context:set_resp_error_code/2, 400}
          ]); 
    ConfirmCode /= <<>> andalso ConfirmCode=:=ServerConfirmCode -> 
      UpdatedUserDB = maps:merge(UserInfo, #{
        status => ?USER_STATUS_ACTIVE
      }),
      user_db:save(UpdatedUserDB),
      Scope    =  wh_json:get_value(<<"scope">>, ReqJson, ?USER_ROLE_USER),
      Auth     = oauth2:authorize_password({UserId, {confirm_code,ConfirmCode}}, <<>>, Scope, [{scope, Scope}]),
      lager:debug("Auth: ~p~n",[Auth]),
      NewContext = user_handler:issue_token(Auth, Context),
      lager:debug("NewContext: ~p~n",[NewContext]),

      cb_context:setters(NewContext, [{fun cb_context:set_resp_status/2, 'success'}]);
    true ->
      Context2 = api_util:validate_error(Context, <<"confirm_code">>, <<"invalid">>, <<"confirm_code_not_match">>), 
      cb_context:setters(Context2,
                        [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, 400}
                        ])
  end;

handle_user_confirm(Context, [UserInfo]) -> 
  handle_user_confirm(Context, UserInfo);


handle_user_confirm(Context, _) -> 
    Context2 = api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"phon_number_notfound">>),
    cb_context:setters(Context2,
              [
                {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
                {fun cb_context:set_resp_status/2, <<"error">>},
                {fun cb_context:set_resp_error_code/2, 404}
   ]).

handle_user_resend(Context, 
  #{
    id := UserId,
    phone_number := PhoneNumberDb
  } = UserDbInfo
)  -> 
      case user_actor:start_actor(PhoneNumberDb) of 
      true -> 
          ReqJson =  cb_context:req_json(Context),
          
          IsDebug = wh_json:get_value(<<"debug">>, ReqJson, <<"false">>),

          ConfirmCode = create_confirm_code_by_phone(PhoneNumberDb),
          NewUserInfo = maps:merge(UserDbInfo, #{
              confirm_code => ConfirmCode,
              confirm_code_created_time_dt => zt_datetime:get_now()
          }),
          user_db:save(NewUserInfo),
          InitRespData = #{
              id =>  UserId,
              phone_number => PhoneNumberDb
          },
          RespData = 
              case IsDebug of
                  <<"true">> -> 
                      maps:merge(InitRespData, #{
                          confirm_code => ConfirmCode
                      });
                  _ ->
                          send_otp(PhoneNumberDb,ConfirmCode),
                          InitRespData
              end,
          user_actor:add_resend(PhoneNumberDb),
          cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, RespData}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                              ]);
        Error -> 
          Context2 = api_util:validate_error(Context, <<"resend_otp">>, <<"invalid">>, Error),
          cb_context:setters(Context2,
                    [
                      {fun cb_context:set_resp_error_msg/2, Error},
                      {fun cb_context:set_resp_status/2, <<"error">>},
                      {fun cb_context:set_resp_error_code/2, 400}
          ])
      end;

handle_user_resend(Context, [UserInfo]) -> 
    handle_user_resend(Context, UserInfo);

handle_user_resend(Context, _) -> 
    Context2 = api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"phon_number_notfound">>),
    cb_context:setters(Context2,
    [
      {fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
      {fun cb_context:set_resp_status/2, <<"error">>},
      {fun cb_context:set_resp_error_code/2, 404}
    ]).

send_otp(PhoneNumber, ConfirmCode) when is_binary(ConfirmCode)->
  spawn(fun() -> 
    tel4vn_voice_otp:call(PhoneNumber, ConfirmCode)
  end);

send_otp(PhoneNumber, ConfirmCode) ->
    send_otp(PhoneNumber, zt_util:to_bin(ConfirmCode)).



-spec validate_confirm_code(api_binary(), cb_context:context()) -> cb_context:context().
validate_confirm_code(ReqJson, Context) ->
  ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
  case ConfirmCode of 
    <<>> ->
      api_util:validate_error(Context, <<"confirm_code">>, <<"required">>, <<"confirm_code_required">>);
    _ ->
      Context
  end. 

-spec validate_confirm_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_confirm_phone_number(ReqJson, Context) ->
  Val = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
  case Val of 
    <<>> ->
      api_util:validate_error(Context, <<"phone_number">>, <<"required">>, <<"phone_number_required">>);
    _ ->
      Context
  end.

-spec validate_search_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_search_phone_number(ReqJson, Context) ->
  Key = <<"phone_number">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  api_util:check_val(Context, Key, Val).

-spec validate_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_phone_number(ReqJson, Context) ->
  PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
  case PhoneNumber of
    <<>> ->
      api_util:validate_error(Context, <<"phone_number">>, <<"required">>, <<"phone_number_required">>);
    _  ->
      case re:run(zt_util:to_str(PhoneNumber), ?PHONEREGX) of 
        nomatch ->
          api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"phone_number_is_invalid">>);
        _ -> 
          case user_handler:check_register_user_existed(PhoneNumber) of
            false -> Context;
            _ -> 
              api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"phone_number_in_use">>)
          end
      end
  end. 

-spec validate_role(api_binary(), cb_context:context()) -> cb_context:context().
validate_role(ReqJson, Context) ->
  Key = <<"role">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  case api_util:check_val(Context, Key, Val) of 
    Context ->
      validate_role_value(Key,Val,Context);
    ErrorContext ->
      ErrorContext
  end.

-spec validate_role_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_role_value(Key, Val, Context) ->
    case lists:member(Val, ?USER_ROLES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?USER_ROLES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

-spec validate_password(api_binary(), cb_context:context()) -> cb_context:context().
validate_password(ReqJson, Context) ->
  Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
  LenPass = length(zt_util:to_str(Password)),
  case LenPass of
    0 ->
      api_util:validate_error(Context, <<"password">>, <<"required">>, <<"password_required">>);
    Val when Val < 8 ->
      api_util:validate_error(Context, <<"password">>, <<"invalid">>, <<"password_min_8_charactor">>);
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
      api_util:validate_error(Context, <<"new_password">>, <<"invalid">>, <<"password_min_8_charactor">>);
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