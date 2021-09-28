-module(cb_confirm_code).

-include("crossbar.hrl").

-export([init/0
	,allowed_methods/0
	,authorize/1
	,authorize/2
	,validate/1
	,resource_exists/0
	,handle_post/1
	,authenticate/1
]).

-export([
    permissions/0
  ]).

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.confirm_code">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.confirm_code">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate.confirm_code">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.confirm_code">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.confirm_code">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.confirm_code">>, ?MODULE, 'handle_post').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
%% /api/v1/confirm_code
allowed_methods() ->
	[?HTTP_POST].


-spec resource_exists() -> 'true'.
%% /api/v1/confirm_code
resource_exists() -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) ->  true.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context) ->
    true.

authorize(_Context, _Id) ->
    true. 
    
-spec validate(cb_context:context()) ->  cb_context:context().
%% Validate resource : /api/v1/confirm_code
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% POST api/v1/confirm_code
-spec handle_post(cb_context:context()) -> cb_context:context().
handle_post(Context) ->

	ReqJson =  cb_context:req_json(Context),
   	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
	   case otp_actor:start_actor(PhoneNumber) of 
			true -> 
					ConfirmCode = user_handler:create_confirm_code_by_phone(PhoneNumber),
					Info = #{
						phone_number => PhoneNumber,
						confirm_code => ConfirmCode,
						created_time => zt_datetime:get_now()
					},
					phone_number_db:save(Info),
					user_handler:send_otp(PhoneNumber,ConfirmCode),
					otp_actor:add_resend(PhoneNumber),
					RespData = #{
						phone_number => PhoneNumber,
						expired_duration => otp_handler:get_otp_expired_duration()
					},
					
					cb_context:setters(Context
										,[{fun cb_context:set_resp_data/2, RespData}
										,{fun cb_context:set_resp_status/2, 'success'}]);
			OtpError ->
					Context2 = api_util:validate_error(Context, <<"otp">>, <<"invalid">>, OtpError),
					cb_context:setters(Context2,[
											{fun cb_context:set_resp_error_msg/2, OtpError},
											{fun cb_context:set_resp_status/2, <<"error">>},
											{fun cb_context:set_resp_error_code/2, 400}
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
					   fun validate_phone_number/2
					],
		lists:foldl(fun(F, C) ->
						F(ReqJson, C)
					end, Context1,  ValidateFuns);

validate_request(Context, _) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
								  ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [
		fun validate_phone_number/2
	],
	lists:foldl(fun(F, C) ->
			F(ReqJson, C)
	end, Context1,  ValidateFuns).

validate_phone_number(ReqJson, Context) ->
	Key = <<"phone_number">>,
	Val = wh_json:get_value(Key, ReqJson, <<>>),
	api_util:check_val(Context, Key, Val).