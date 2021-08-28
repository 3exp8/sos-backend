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
   	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	lager:debug("email confirm ReqJson: ~p~n",[ReqJson]),
   	case user_db:find_by_email(Email) of 
   	[#{
		   id := UserId, 
		   status := Status,
		   first_name := FirstName,
		   last_name := LastName,
		   confirm_code := ConfirmCodeServer,
		   confirm_code_created_time_dt := ConfirmCodeCreatedTime
		} = Account | _] ->

		CurrentTimeToSecond = zt_util:timestamp_second(),
		ConfirmCodeCreatedTimeToSecond = zt_util:datetime_binary_to_second(ConfirmCodeCreatedTime),
		case Status of
		?INACTIVE ->
			ConfirmCode = 
			if 
				CurrentTimeToSecond - ConfirmCodeCreatedTimeToSecond > ?DATESECOND  
				orelse  ConfirmCodeServer == <<>>  ->
					NewConfirmCode = zt_util:create_random_number(), 
					NewConfirmCodeCreatedTime =  zt_datetime:get_now(),
					UserDb = maps:merge(Account, #{
						confirm_code => NewConfirmCode,
						confirm_code_created_time_dt => NewConfirmCodeCreatedTime
					}),
					user_db:save(UserDb),
					NewConfirmCode ;
				true ->
					ConfirmCodeServer 
			end, 
			spawn(fun() ->  
				app_util:send_email({resend_confirm_code, Context, UserId, Email, FirstName, LastName, ConfirmCode, host, 80}) 
			end),
			  %% send confirm code to email
			cb_context:setters(Context
		                       ,[{fun cb_context:set_resp_status/2, 'success'}]);
		_ ->
			cb_context:setters(Context,
    			[{fun cb_context:set_resp_error_msg/2, <<"Account Actived">>},
    			 {fun cb_context:set_resp_status/2, <<"error">>},
                 {fun cb_context:set_resp_error_code/2, 400}
                ])
		end;
	Other ->
		lager:debug("email confirm other: ~p~n",[Other]),
		cb_context:setters(Context,
    			[{fun cb_context:set_resp_error_msg/2, <<"Email Not Found">>},
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
validate_request(Context, _Verb) ->
	lager:info("validate_account: ~n",[]),
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),	
	case Email of
		<<>> ->
			api_util:validate_error(Context, <<"email">>, <<"required">>, <<"Field 'email' is required">>);
        _ ->
        	case re:run(zt_util:to_str(Email), ?EMAILREGX)  of 
        		nomatch ->
        			api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
        		_ ->
        			Context1 
			end
	end.


