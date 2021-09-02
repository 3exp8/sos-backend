-module(user_handler).

-include("crossbar.hrl").
-export([
      check_register_user_existed/1
    , find_unconfirmed_user/1
    , handle_user_confirm/2
    , handle_user_resend/2
    , send_otp/2
    , create_confirm_code_by_phone/1
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
  if 
    EplasedSeconds > 120 ->
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
      NewContext = cb_customers:issue_token(Auth, Context),

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
      case user_actor:check_otp_rule(PhoneNumberDb) of 
      true -> 
          ReqJson =  cb_context:req_json(Context),
          
          IsDebug = wh_json:get_value(<<"debug">>, ReqJson, <<"true">>),

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
                          user_handler:send_otp(PhoneNumberDb,ConfirmCode),
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
    zt_r2r_util:send_sms(?DEFAULT_SENDER_PHONE_NUMBER,PhoneNumber,ConfirmCode);

send_otp(PhoneNumber, ConfirmCode) ->
    send_otp(PhoneNumber, zt_util:to_bin(ConfirmCode)).