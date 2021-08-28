-module(cb_client).

-include("crossbar.hrl").

-export([init/0
	,allowed_methods/0
	,allowed_methods/1
	,authorize/1
    ,authorize/2
	,validate/1
	,validate/2
	,resource_exists/0
	,resource_exists/1
	,authenticate/1
	,authenticate/2
	,handle_get/1
	,handle_get/2
	,handle_put/1
	]).

-export([
          permissions/0
  ]).

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.client">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.client">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.client">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authenticate.client">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.client">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.client">>, ?MODULE, 'handle_get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.client">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().


%% /api/v1/client
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/client/clientid
allowed_methods(_ClientId) ->
	[?HTTP_GET].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.


%% /api/v1/client
resource_exists() -> 'true'.

% /api/v1/client/clientid
resource_exists(_ClientId) -> 'true'.


%% /api/v1/client
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) -> 
	lager:info("authenticate: 1 params",[]),
	Token = cb_context:auth_token(Context),
	zt_util:oauth2_authentic(Token, Context).

%% /api/v1/client/clientid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _ClientId) ->
	lager:info("authenticate: 2 params",[]),
	Token = cb_context:auth_token(Context),
	lager:info("authenticate: 2 params: ~p ~n",[Token]),
	zt_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context) ->
    true.

authorize(_Context, _Id) ->
    true. 

-spec validate(cb_context:context()) ->  cb_context:context().
% -spec validate(cb_context:context(), path_token()) ->  cb_context:context().


%% Validate resource : /api/v1/client
validate(Context) ->
    validate_client(Context, cb_context:req_verb(Context)).


% Validate resource : /api/v1/client/clientid
validate(Context, ClientId) ->
    validate_client(Context, ClientId, cb_context:req_verb(Context)).



-spec validate_client(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_client(cb_context:context(), path_token(), http_method()) -> cb_context:context().


%% PUT /api/v1/client
validate_client(Context, ?HTTP_PUT = Verb) ->
	lager:info("client: validate request: PUT ~n",[]),
	validate_request(Context, Verb);

%% GET /api/v1/client
validate_client(Context, ?HTTP_GET = Verb) ->
	lager:info("client: validate request: GET: ~n",[]),
	%cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]).
	validate_request(Context, Verb).

%% POST /api/v1/client/clientid
validate_client(Context, ClientId, ?HTTP_POST = Verb) ->
	lager:info("client: validate request: POST ~n",[]),
	validate_request(ClientId, Context, Verb);

%% GET /api/v1/client/clientid
validate_client(Context, ClientId, ?HTTP_GET = Verb) ->
	lager:info("client: validate request: GET ~n",[]),
	validate_request(ClientId, Context, Verb).


-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().

%% GET api/v1/client

handle_get({Req, Context}) ->
	
	lager:info("handle_get: ~n",[]),
	{QueryJson} = cb_context:query_string(Context),
	RoleAccountOwnerToken = cb_context:role(Context),
	AccountId = wh_json:get_value(<<"account_id">>, {QueryJson}, <<>>),
	%AccountId = cb_context:account_id(Context),
	Limit = binary_to_integer(wh_json:get_value(<<"limit">>, {QueryJson}, <<"0">>)),
	Offset = binary_to_integer(wh_json:get_value(<<"offset">>, {QueryJson}, <<"0">>)),


	try 
		Clients = 

		if
	   	 AccountId /= <<>> -> get_clients_by_account_id(RoleAccountOwnerToken, AccountId, QueryJson, Limit, Offset);
        			true -> get_clients(RoleAccountOwnerToken, QueryJson, Limit, Offset)

		end,

		case Clients of
	        	{error, _} ->
					{Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
								   {fun cb_context:set_resp_status/2, <<"error">>},
								   {fun cb_context:set_resp_error_code/2, 403}])};
	    		_ ->

					PropClients = lists:map(fun(Client) ->  get_sub_fields_clients(Client) end, Clients),
								{Req, cb_context:setters(Context
		                       			,[{fun cb_context:set_resp_data/2, PropClients}
		                         			,{fun cb_context:set_resp_status/2, 'success'}])}
		end

	catch
		_:_ -> {Req, cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'error'}
						 ,{fun cb_context:set_resp_error_code/2, <<"400">>}
						 ,{fun cb_context:set_resp_error_msg/2, <<"Bad request">>}
                        ])}
	end.	 


% GET api/v1/client/clientid
handle_get({Req, Context}, ClientId) ->
	
	AccountId = cb_context:account_id(Context),
	Role = cb_context:role(Context),

	Context1 = 
	case client_db:find_client(ClientId) of 
	#{account_id := AccountIdDb} = Client ->
		if 
		Role == ?USER_ROLE_ADMIN;
		AccountId == AccountIdDb -> 
			PropClient = get_sub_fields_clients(Client),
			lager:info("PropAccount: handle_get: ~p ~n",[PropClient]),
			cb_context:setters(Context
		                       ,[{fun cb_context:set_resp_data/2, PropClient}
		                         ,{fun cb_context:set_resp_status/2, 'success'}
		                        ]);
		true ->
			cb_context:setters(Context,
	    			[{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
	    			 {fun cb_context:set_resp_status/2, <<"error">>},
	                 {fun cb_context:set_resp_error_code/2, 403}
	                ])
		end; 
	_ ->
		cb_context:setters(Context,
    			[{fun cb_context:set_resp_error_msg/2, <<"Non Exist Request_Id">>},
    			 {fun cb_context:set_resp_status/2, <<"error">>},
                 {fun cb_context:set_resp_error_code/2, 400}
                ])
	end,
	{Req, Context1}.

%% PUT api/v1/client
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
	
	AccountId = cb_context:account_id(Context),
	AccountRole = cb_context:role(Context),
	Host =  cb_context:raw_host(Context),
    Port = cb_context:raw_port(Context),
    ReqJson = cb_context:req_json(Context),
	ClientName = wh_json:get_value(<<"client_name">>, ReqJson, <<>>) ,
    Email  = wh_json:get_value(<<"email">>, ReqJson, <<>>), 
	Logo = wh_json:get_value(<<"logo">>, ReqJson, <<>>),
	UrlInfo = wh_json:get_value(<<"url_info">>, ReqJson, <<>>),
	Description = wh_json:get_value(<<"description">>, ReqJson, <<>>),
	RedirectUrl = wh_json:get_value(<<"redirect_uri">>, ReqJson, <<>>),
	ClientId = zt_util:get_uuid(),
	SecretKey = zt_util:get_uuid(?CODE_SIZE),
	CreatedTime = zt_datetime:get_now(),
	UpdatedTime = CreatedTime,

	Client = #{
			client_id => ClientId,
			secret_key => SecretKey,
			account_id => AccountId,
			account_role => AccountRole,
			client_name => ClientName,
			email => Email,
			logo => Logo,
			url_info => UrlInfo,
			description => Description,
			redirect_uri => RedirectUrl,
			created_by => Email, 
			created_time_dt => CreatedTime,
			updated_time_dt => UpdatedTime
			},

	client_db:save_client(Client),

	RespData = [{<<"account_id">>, AccountId}, {<<"client_id">>, ClientId}, {<<"secret_key">>, SecretKey}],
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, RespData}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]).


permissions() ->
  authorize_util:default_permission(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

%% PUT api/v1/client
validate_request(Context, ?HTTP_PUT) ->
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [
					fun validate_client_name/1
					,fun validate_email/1
					],
	lists:foldl(fun(F, C) ->
			F(C)
	end, Context1,  ValidateFuns);


%%GET api/v1/client
validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, _Verb) ->
	Context.



%%GET api/v1/client/clientid
-spec  validate_request(path_token(), cb_context:context(), http_method()) -> cb_context:context().
validate_request(ClientId, Context, ?HTTP_GET) ->
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]),	

	ValidateFuns = [fun validate_exist_client/2],
	lists:foldl(fun(F, C) ->
			F(ClientId, C)
	end, Context1,  ValidateFuns).
	

-spec validate_exist_client(api_binary(), cb_context:context()) -> cb_context:context().
validate_exist_client(ClientId, Context) -> 

	lager:info("exist client: ~p ~n",[ClientId]),
	case client_db:find_client(ClientId) of 
	#{} = Client ->
		cb_context:setters(Context
                       ,[{fun cb_context:set_account_doc/2, Client}]);
	_ ->
		api_util:validate_error(Context, <<"client_id">>, <<"invalid">>, <<"Client Not found">>)
	end. 


-spec validate_client_name(cb_context:context()) -> cb_context:context().
validate_client_name(Context) ->
	lager:info("validate_company_name: ~n",[]),
	ReqJson = cb_context:req_json(Context),
	ClientName = wh_json:get_value(<<"client_name">>, ReqJson, <<>>),
	case ClientName of
		<<>> ->
			api_util:validate_error(Context, <<"client_name">>, <<"required">>, <<"Field 'client_name' is required">>);
		_  ->
			Context
	end. 

-spec validate_email(cb_context:context()) -> cb_context:context().
validate_email(Context) ->
	lager:info("validate_company_address: ~n",[]),
	ReqJson = cb_context:req_json(Context),
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	case Email of
		<<>> ->
			api_util:validate_error(Context, <<"email">>, <<"required">>, <<"Field 'email' is required">>);
		_  ->
			case re:run(zt_util:to_str(Email), ?EMAILREGX) of 
    		nomatch ->
    			api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
    		_ -> 
				case is_client_exist(Email) of
						false -> Context;
						_ -> api_util:validate_error(Context, <<"email">>, <<"unique">>, <<"Email already in use">>)
				end
			end
	end. 

get_clients_by_account_id(RoleAccountOwnerToken, AccountId, QueryJson, Limit, Offset) ->
	if 
		RoleAccountOwnerToken == ?USER_ROLE_USER ->
			lager:info("nhinhi user role get clients ~n", []),
			client_db:find_by_conditions([{account_id, AccountId}], QueryJson, Limit, Offset) ;

		RoleAccountOwnerToken == ?USER_ROLE_ADMIN ->
			lager:info("nhhinhi admin role get clients ~n", []),
			RespData = client_db:find_by_conditions([], QueryJson, Limit, Offset),
			case RespData of
				{error,_} ->
	    					[];
	    				
				Client ->
	    			        client_db:find_by_conditions([], QueryJson, Limit, Offset)
    			end;

		
		true ->
				{error, forbidden}
	end.

get_clients(RoleAccountOwnerToken, QueryJson, Limit, Offset) ->
	if 
		RoleAccountOwnerToken == ?USER_ROLE_ADMIN ->
			lager:info("nhinhi only admin get clients ~n", []),
			RespData = client_db:find_by_conditions([], QueryJson, Limit, Offset),
			case RespData of
				{error,_} ->
	    					[];
	    				
				Client ->
	    			        client_db:find_by_conditions([], QueryJson, Limit, Offset)
    			end;

		
		true ->
				{error, forbidden}
	end.


get_sub_fields_clients(Client) ->
	Res = maps:to_list(Client),
	proplists:substitute_aliases([
								{created_time_dt, created_time}
								,{updated_time_dt, updated_time}], Res).

-spec is_client_exist(ne_binary()) ->  boolean().
is_client_exist(Email) ->
	case catch client_db:find_client_by_email(Email) of
		[Val| _]  when is_map(Val) ->
			true;
		_ ->
			false
	end. 
