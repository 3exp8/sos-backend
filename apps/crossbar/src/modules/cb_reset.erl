-module(cb_reset).

-include("crossbar.hrl").

-export([init/0
	,allowed_methods/0
	,validate/1
	,resource_exists/0
	,handle_post/1
	,authenticate/1
	,authorize/1
	]).

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.reset">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.reset">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authenticate.reset">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.reset">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.reset">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.reset">>, ?MODULE, 'handle_post').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
%% /api/v1/reset
allowed_methods() ->
	[?HTTP_POST].


-spec resource_exists() -> 'true'.
%% /api/v1/reset
resource_exists() -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) ->  true.

-spec authorize(cb_context:context()) -> boolean().
authorize(_Context) ->
    true.


-spec validate(cb_context:context()) ->  cb_context:context().
%% Validate resource : /api/v1/reset
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% POST api/v1/reset
-spec handle_post(cb_context:context()) -> cb_context:context().
handle_post(Context) ->
	ReqJson =  cb_context:req_json(Context),
   	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
   	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
   	ConfirmCode= wh_json:get_value(<<"confirm_code">>,  ReqJson, <<>>), 
   	case user_db:find_by_phone_number(PhoneNumber) of 
   	[#{
		   id := Id, 
		   role := RoleDb,
		   confirm_code := ConfirmCodeServer, 
		   confirm_code_created_time_dt:= ConfirmCodeCreatedTimeServer
		} = Info | _] -> 
		ConfirmCodeCreatedTimeServerToSecond = zt_util:datetime_binary_to_second(ConfirmCodeCreatedTimeServer),
		OtpExpiredDuration = zt_util:to_integer(application:get_env(crossbar, otp_expired_duration, 120)),

		EplasedSeconds = zt_datetime:diff_second(ConfirmCodeCreatedTimeServer),
		if 
			EplasedSeconds > OtpExpiredDuration ->
				cb_context:setters(Context,
									[{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
									{fun cb_context:set_resp_status/2, <<"error">>},
									{fun cb_context:set_resp_error_code/2, 400}
									]);

		ConfirmCode =:= ConfirmCodeServer  ->
			{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
			{ok, NewPassHash} = bcrypt:hashpw(Password, Salt),
			UserDb = maps:merge(Info, #{
					password => zt_util:to_bin(NewPassHash),
					updated_time_dt => zt_datetime:get_now()
			}),
			user_db:save(UserDb), 
			api_doc:del_tokens_of_user(Id),
			Scope    =  wh_json:get_value(<<"scope">>, ReqJson, ?USER_ROLE_USER),
			Auth     = oauth2:authorize_password({Id, {confirm_code,ConfirmCode}}, <<>>, Scope, [{scope, Scope}]),
			lager:debug("Auth: ~p,RoleDb: ~p~n",[Auth, RoleDb]),
			ContextWithRole = cb_context:set_role(Context,RoleDb),
			lager:debug("ContextWithRole: ~p~n",[ContextWithRole]),

			NewContext = user_handler:issue_token(Auth, ContextWithRole),
			cb_context:setters(NewContext
                       ,[{fun cb_context:set_resp_status/2, 'success'}]);
		true ->
			cb_context:setters(Context,
        			[{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
        			 {fun cb_context:set_resp_status/2, <<"error">>},
                     {fun cb_context:set_resp_error_code/2, 400}
                    ])
		end ;
	_ ->
		cb_context:setters(Context,
        			[{fun cb_context:set_resp_error_msg/2, <<"Phone number Not Found">>},
        			 {fun cb_context:set_resp_status/2, <<"error">>},
                     {fun cb_context:set_resp_error_code/2, 404}
                    ])
	end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

validate_request(Context, _Verb) ->
	lager:info("validate reset password: ~n",[]),
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [
						fun user_handler:validate_confirm_phone_number/2
						,fun user_handler:validate_password/2
						,fun user_handler:validate_confirm_code/2
	],
	lists:foldl(fun(F, C) ->
			F(ReqJson, C)
	end, Context1,  ValidateFuns).