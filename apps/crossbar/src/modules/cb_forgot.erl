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
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context) ->
    true.

authorize(_Context, _Id) ->
    true. 
    
-spec validate(cb_context:context()) ->  cb_context:context().
%% Validate resource : /api/v1/forgot
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% POST api/v1/forgot
-spec handle_post(cb_context:context()) -> cb_context:context().
handle_post(Context) ->
	Host =  cb_context:raw_host(Context),
    Port = cb_context:raw_port(Context),
	ReqJson =  cb_context:req_json(Context),
   	Email = wh_json:get_value(<<"email">>, ReqJson),
   	case user_db:find_by_email(Email) of 
   	[#{id := AccountId, status := Status} = Account | _]  -> 
		case Status of
			?ACTIVE ->
				ConfirmCode = zt_util:create_random_number(),
        		lager:info("CONFIRM_CODE ~p ~n", [ConfirmCode]),
				ConfirmCodeCreatedTime = zt_datetime:get_now(),
				lager:info("nhit ~n",[]), 
				spawn(fun() ->  app_util:send_email({'forgot', Context, AccountId, Email, ConfirmCode, Host, Port}) end),
				UserDb = maps:merge(Account, #{confirm_code => ConfirmCode, 
												confirm_code_created_time_dt => ConfirmCodeCreatedTime}),
				user_db:save(UserDb),
        lager:info("USERDB ~p ~n", [UserDb]),
				cb_context:setters(Context
			                       ,[{fun cb_context:set_resp_status/2, 'success'}]);
			_ ->
				cb_context:setters(Context,
	    			[{fun cb_context:set_resp_error_msg/2, <<"InActive Account">>},
	    			 {fun cb_context:set_resp_status/2, <<"error">>},
	                 {fun cb_context:set_resp_error_code/2, 400}
	                ])
		end;
	_ -> 
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
        	case re:run(zt_util:to_str(Email), ?EMAILREGX) of 
        		nomatch ->
        			api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
        		_ ->
        			Context1  
			end
	end.
	
