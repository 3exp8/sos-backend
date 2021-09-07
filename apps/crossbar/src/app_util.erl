-module(app_util).

-include("crossbar.hrl").
-include_lib("db/include/db.hrl").

-export([oauth2_authentic/1
         ,oauth2_authentic/2
         ,send_email/1
         ,send_email_response_callback/1
         ,phone_number/1
         ,request_id/1
         ,trunc/2
         ,round_number/1
         ,get_role/1
         ,validate_key/5
         ,key_name/2
        ]).

-export([
         check_phonenumber/3,
         validate_timezone/3,
         create_random_number/0,
         get_requester_id/1,
         create_module_validates/1,
         declare_api_validate/3
        ]).


create_module_validates(Apis) -> 
    lists:foldl(fun(#{
          method := Method,
          path := Path,
          validates := Validates
      },ValidateAcc) -> 
          ValiateResponses = create_validates(Validates),
            ValidateMap = #{
              method => Method,
              path => Path,
              validate_reponses => ValiateResponses
          },
          [ValidateMap|ValidateAcc]
    end, [], Apis).
                    
  create_validates(Validates) -> 
                      Context = cb_context:new(), 
                    Context2 =
                      lists:foldl(fun({Key, Type, Msg}, Ctx) -> 
                          api_util:validate_error(Ctx, Key, Type, Msg)
                      end, Context, Validates),
                      cb_context:validation_errors(Context2).
  
  
  declare_api_validate(Method, Path, Validates) -> 
  #{
    method => Method,
    path => Path,
    validates => Validates
  }.

-spec create_random_number() -> binary().
create_random_number() ->
                      Min = 1000,
                      Max = 9999,
                      zt_util:to_bin(Min + rand:uniform(Max-Min)).

check_phonenumber(Context, Key, Phone) ->
  case Key of
    <<>> ->
      api_util:validate_error(Context, Key, <<"required">>, <<"Field '", Key/binary, "' is required">>);
    _ ->
      case re:run(zt_util:to_str(Phone), ?PHONEREGX) of
        nomatch ->
          api_util:validate_error(Context, Key,  <<"invalid">>, <<"Invalid '", Key/binary, "'">>);
        _ ->
          Context
      end
  end.


validate_timezone(Context, Key, <<>>) ->
  api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid TimeZone">>);

validate_timezone(Context, Key, TimeZone) ->
  case qdate:to_date(TimeZone, prefer_daylight, <<"2021-06-25T10:20:30+07:00:00Z">>) of 
          {error,Error} ->
              lager:error("Error timezone validate with error: ~p~n",[Error]),
              api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid TimeZone">>);
          _ -> 
            Context
  end.

oauth2_authentic(Token) ->
  case oauth2:verify_access_token(Token, []) of
    {ok, _Identity} ->
      true;
    _  ->
      false
  end.


oauth2_authentic(Token, Context) ->
  ReqJson =  cb_context:req_json(Context),
  ReqVerb = cb_context:req_verb(Context),
  lager:debug("oauth2_authentic ReqVerb: ~p ReqJson: ~p~n",[ReqVerb, ReqJson]),
  case oauth2:verify_access_token(Token, []) of
    {ok, {_, AccessTokenInfo}} ->
      lager:info("_Identity: ~p ~n",[AccessTokenInfo]),
      AccountId = proplists:get_value(<<"account_id">>, AccessTokenInfo, <<>>),
      UserId = proplists:get_value(<<"user_id">>, AccessTokenInfo, <<>>),
      Roles = proplists:get_value(<<"roles">>, AccessTokenInfo, []),
      Role = proplists:get_value(<<"role">>, AccessTokenInfo, []),
      NewContext =
      cb_context:setters(Context, [{fun cb_context:set_account_id/2, AccountId}
                                   ,{fun cb_context:set_customer_id/2, UserId}
                                   ,{fun cb_context:set_user_id/2, UserId}
                                   ,{fun cb_context:set_roles/2, Roles}
                                   ,{fun cb_context:set_role/2, Role}
                                  ]),
      {true, NewContext};
    _  ->
      lager:info("Fail: ",[]),
      false
  end.


send_email({create, _Context, _AccountId, ToEmail, _FirstName, _LastName, ConfirmCode, _Host, _Port}) ->
  lager:debug("Send user confirm code with email to: ~p ~n", [ToEmail]),
  FromEmail = zt_util:to_bin(?EMAIL_FROM_ADDRESS),
  
  Content = <<"Your confirmation code is ",ConfirmCode/binary,".",
              "\r\nPlease enter this code in to finish setting up your account.\r\n">>,

  Subject =  <<"Welcome to Covid SOS!">>,
  zt_r2r_util:send_email(FromEmail, ?EMAIL_FROM_NAME, ToEmail, Subject, Content);

send_email({create_nouse, _Context, AccountId, ToEmail, FirstName, LastName, ConfirmCode, Host, Port}) ->
  lager:debug("Send user confirm code with email to: ~p ~n", [ToEmail]),
  FromEmail = zt_util:to_bin(?EMAIL_FROM_ADDRESS),
  Url = zt_util:to_bin(make_url(Host, Port, AccountId, ConfirmCode, ?CREATE_CONFIRM_RESOUCE)),

  FullName = case FirstName of
               <<>> -> LastName;
               _ -> FirstName
             end,
  Content = <<"Hi ",FullName/binary,",\r\n\r\nWe've created an account for you. "
              ,"\r\nPlease follow the link below to confirm your account and gain access our system.\r\n"
              ,Url/binary,"\r\n\r\nSincerely,\r\n SOS Team">>,

  Subject =  <<"Welcome to SOS Team!">>,
  zt_r2r_util:send_email(FromEmail, ?EMAIL_FROM_NAME, ToEmail, Subject, Content);

send_email({resend_confirm_code, _Context, _AccountId, ToEmail, _FirstName, _LastName, ConfirmCode, _Host, _Port}) ->
  lager:debug("Resend user confirm code with email to: ~p ~n", [ToEmail]),
  FromEmail = zt_util:to_bin(?EMAIL_FROM_ADDRESS),
  
  Subject =  <<"Code Confirmation">>,
    Content = <<"Your confirmation code is ",ConfirmCode/binary,".",
  "\r\nPlease enter this code to finish setting up your account.\r\n">>,
  zt_r2r_util:send_email(FromEmail, ?EMAIL_FROM_NAME, ToEmail, Subject, Content);

send_email({forgot, _Context, _AccountId, ToEmail, ConfirmCode, _Host, _Port}) ->
    lager:debug("send forgot password email to: ~p ~n", [ToEmail]),
    FromEmail = zt_util:to_bin(?EMAIL_FROM_ADDRESS),
    Content = <<"Your confirmation code is ",ConfirmCode/binary,".",
              "\r\nPlease enter this code to reset your password.\r\n">>,
    Subject =  <<"Forgot your password">>, 
   zt_r2r_util:send_email(FromEmail, ?EMAIL_FROM_NAME, ToEmail, Subject, Content);

send_email({email, ToEmail, FromEmail, _FromName, Subject, Content, ConfirmUrlTemplate}) ->
  NewContent = << Content/binary, "\r\n\r\n", ConfirmUrlTemplate/binary>>,
  lager:info("NewContent: ~p~n",[NewContent]),
  zt_r2r_util:send_email(FromEmail, ?EMAIL_FROM_NAME, ToEmail, Subject, NewContent);

send_email(_Patt) -> ok.


send_email_response_callback({error, ErrMsg, EmailInfo}) ->
  lager:error("Send email error From: Err Report: ~p for email: ~p", [ErrMsg, EmailInfo]);

send_email_response_callback({ok, Receipt, EmailInfo}) ->
  lager:info("Send email sucess Receipt: ~p for email: ~p",[Receipt, EmailInfo]);

send_email_response_callback(Msg) ->
  lager:info("Send Email callback message:  ~p",[Msg]).

make_url(_Host, Port, AccountVal, ConfirmCode,  UrlResource) ->
  BPort =  list_to_binary(integer_to_list(Port)),
  PortalUrl = ?PORTAL_URL,
  Url = <<PortalUrl/binary, "/", UrlResource/binary,"/", AccountVal/binary, "/", ConfirmCode/binary>>,
  lager:info("Port: ~p; URl: ~p",[BPort, Url]),
  Url.

phone_number(PhoneNumber) ->
  case PhoneNumber of
    << "+", Num/binary >> ->
      Num ;
    << "00", Num/binary>> ->
      Num;
    _ ->
      binary:replace(PhoneNumber, <<" ">>, <<>>, [global]) %% strim phone number
  end.

request_id(ReqJson) ->
  RequestId = wh_json:get_value(<<"request_id">>, ReqJson),
  RequestHttpId = get('callid'),
  if RequestId /= undefined -> RequestId ;
     RequestHttpId /= undefined -> RequestHttpId;
     true -> <<>>
  end.


-spec trunc(float()|integer(), integer()) -> float().
trunc(F, N) ->
  Prec = math:pow(10, N),
  trunc(F*Prec)/Prec.

round_number(X) ->
  F = zt_util:to_float(X),
  F3 = trunc(F, 3),
  zt_util:to_float(float_to_list(F3,[{decimals, 2}])).

get_role(Context) ->
  Roles = cb_context:roles(Context),
  case Roles of
    [_|_] -> ?USER_ROLE_USER;
    _ -> ?USER_ROLE_CUSTOMER
  end.


key_name(<<>>, Key) ->
	Key;

key_name(Prefix, Key) ->
	<<Prefix/binary,".", Key/binary>>.

%% Validate
validate_key(Module, {object, Key, KN}, Prefix, Value, Context) ->
	case is_list(Value) of
		true ->
			?TO_ATOM(Module, <<"validate_",Key/binary>>, Prefix, {Value}, Context);
		false -> api_util:check_vals(Context, KN, Value)
	end;

validate_key(Module, {{option, {V0, true, V}}, Key, _KN}, Prefix, Value, Context) ->
	if V == V0 ->
			 ?TO_ATOM(Module, <<"validate_",Key/binary>>, Prefix, Value, Context);
		 true ->
			 Context
	end;

validate_key(_Module, {not_null, _, KN}, _Prefix, Value, Context) ->
	api_util:check_val(Context, KN, Value);

validate_key(_Module, {{one_of, L}, _, KN},  _Prefix, Value, Context) ->
	api_util:check_val_one_of(Context, KN, Value, L);

validate_key(Module, {{check_vals_one_of, _L, ReqJson}, Key, _KN},  _Prefix, Value, Context) ->
	?TO_ATOM(Module, <<"validate_",Key/binary>>, Value, ReqJson, Context).

get_requester_id(Context) -> 
  case get_requester_id_from_customer(Context) of 
    <<>> -> get_requester_id_from_user(Context);
    Id -> Id 
  end.

get_requester_id_from_user(Context) -> 
  case cb_context:user_id(Context) of 
    undefined -> <<>>;
    Id -> Id
  end.
  
get_requester_id_from_customer(Context) -> 
  case cb_context:customer_id(Context) of 
      undefined -> <<>>;
      Id -> Id
  end.