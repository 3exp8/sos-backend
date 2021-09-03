-module(cb_forgot).

-include("crossbar.hrl").

-export([init/0
	,allowed_methods/0
	,validate/1
	,resource_exists/0
	,handle_post/1
	,authenticate/1
	,authorize/1
	]).

-export([
          permissions/0
  ]).


init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.forgot">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.forgot">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate.forgot">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.forgot">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.forgot">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.forgot">>, ?MODULE, 'handle_post').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
%% /api/v1/forgot
allowed_methods() ->
	[?HTTP_POST].


-spec resource_exists() -> 'true'.
%% /api/v1/forgot
resource_exists() -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) ->  true.

-spec authorize(cb_context:context()) -> boolean().
authorize(_Context) ->
    true.
    
-spec validate(cb_context:context()) ->  cb_context:context().
%% Validate resource : /api/v1/forgot
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% POST api/v1/forgot
-spec handle_post(cb_context:context()) -> cb_context:context().
handle_post(Context) ->
	ReqJson =  cb_context:req_json(Context),
   	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson),
   	case user_db:find_by_phone_number(PhoneNumber) of 
   	[
		   #{
			   id := AccountId, 
			   status := Status
			} = Account | _]  -> 
		case Status of
			?USER_STATUS_ACTIVE ->
				user_actor:start_actor(PhoneNumber),
				case user_actor:check_otp_rule(PhoneNumber) of 
					true -> 
						ConfirmCode = zt_util:create_random_number(),
						lager:info("CONFIRM_CODE ~p ~n", [ConfirmCode]),
						ConfirmCodeCreatedTime = zt_datetime:get_now(),
						
						UserDb = maps:merge(Account, #{
							confirm_code => ConfirmCode, 
							confirm_code_created_time_dt => ConfirmCodeCreatedTime
						}),
						user_db:save(UserDb),
						user_actor:add_resend(PhoneNumber),
						lager:info("USERDB ~p ~n", [UserDb]),
						InitRespData = maps:with([phone_number],UserDb),
						IsDebug = <<"true">>,
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
					OtpError ->
						Context2 = api_util:validate_error(Context, <<"resend_otp">>, <<"invalid">>, OtpError),
						cb_context:setters(Context2,
							[
							{fun cb_context:set_resp_error_msg/2, OtpError},
							{fun cb_context:set_resp_status/2, <<"error">>},
							{fun cb_context:set_resp_error_code/2, 400}
						])

				end;
			_ ->
				cb_context:setters(Context,
	    			[{fun cb_context:set_resp_error_msg/2, <<"InActive User">>},
	    			 {fun cb_context:set_resp_status/2, <<"error">>},
	                 {fun cb_context:set_resp_error_code/2, 400}
	                ])
		end;
	_ -> 
		cb_context:setters(Context,
    			[{fun cb_context:set_resp_error_msg/2, <<"Phone Number Not Found">>},
    			 {fun cb_context:set_resp_status/2, <<"error">>},
                 {fun cb_context:set_resp_error_code/2, 404}
                ])
    end.  

permissions() ->
  authorize_util:default_permission(?MODULE).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().
validate_request(Context, ?HTTP_POST) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
								  ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [
				   fun user_handler:validate_confirm_phone_number/2
				],
	lists:foldl(fun(F, C) ->
					F(ReqJson, C)
	end, Context1,  ValidateFuns);

validate_request(Context, _) -> Context.
