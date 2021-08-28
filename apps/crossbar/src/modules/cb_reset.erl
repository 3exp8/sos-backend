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
   	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
   	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
   	ConfirmCode= wh_json:get_value(<<"confirm_code">>,  ReqJson, <<>>), 
   	case user_db:find_by_email(Email) of 
   	[#{id := AccountId, confirm_code := ConfirmCodeServer, 
   		confirm_code_created_time_dt:= ConfirmCodeCreatedTimeServer} = Account | _] -> 
	   	UpdatedTime = zt_datetime:get_now(),
	   	CurrentTimeSecond = zt_util:timestamp_second(),
		ConfirmCodeCreatedTimeServerToSecond = zt_util:datetime_binary_to_second(ConfirmCodeCreatedTimeServer),
		if 
		CurrentTimeSecond - ConfirmCodeCreatedTimeServerToSecond >  ?DATESECOND ->
		
			cb_context:setters(Context,
			        			[{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
			        			 {fun cb_context:set_resp_status/2, <<"error">>},
			                     {fun cb_context:set_resp_error_code/2, 400}
			                    ]);

		ConfirmCode =:= ConfirmCodeServer  ->
			{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
			{ok, NewPassHash} = bcrypt:hashpw(Password, Salt),
			UserDb = maps:merge(Account, #{password => zt_util:to_bin(NewPassHash),
											updated_time_dt => UpdatedTime,
											confirm_code => <<>>}),
			user_db:save(UserDb), 
			api_doc:del_tokens_of_user(AccountId),
			cb_context:setters(Context
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
        			[{fun cb_context:set_resp_error_msg/2, <<"Email Not Found">>},
        			 {fun cb_context:set_resp_status/2, <<"error">>},
                     {fun cb_context:set_resp_error_code/2, 404}
                    ])
	end. 
			

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

validate_request(Context, _Verb) ->
	lager:info("validate_account: ~n",[]),
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [fun validate_email/2
					,fun validate_password/2
					,fun validate_confirm_code/2],
	lists:foldl(fun(F, C) ->
			F(ReqJson, C)
	end, Context1,  ValidateFuns).


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

-spec validate_email(api_binary(), cb_context:context()) -> cb_context:context().
validate_email(ReqJson, Context) ->
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),	
	case Email of
		<<>> ->
			api_util:validate_error(Context, <<"email">>, <<"required">>, <<"Field 'email' is required">>);
        _ ->
        	case re:run(zt_util:to_str(Email), ?EMAILREGX) of 
        		nomatch ->
        			api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
        		_ ->
        			Context 
			end
	end.


-spec validate_confirm_code(api_binary(), cb_context:context()) -> cb_context:context().
validate_confirm_code(ReqJson, Context) ->
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),	
	case ConfirmCode of
		<<>> ->
			api_util:validate_error(Context, <<"confirm_code">>, <<"required">>, <<"Field 'confirm_code' is required">>);
        _ ->
			Context
	end.
	
