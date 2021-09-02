-module(cb_auth).

-include("crossbar.hrl").

-export([init/0
				 ,allowed_methods/0
				 ,validate/1
				 ,authenticate/1
				 ,authorize/1
				 ,authorize/2
				 ,resource_exists/0]).


-export([
		handle_get/1
		,handle_post/1
		,errors/0
	]).

init() ->
	_ = crossbar_bindings:bind(<<"*.allowed_methods.auth">>, ?MODULE, 'allowed_methods'),
	_ = crossbar_bindings:bind(<<"*.resource_exists.auth">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.auth">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.auth">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.authorize.auth">>, ?MODULE, 'authorize'),
	_ = crossbar_bindings:bind(<<"*.to_json.get.auth">>, ?MODULE, 'handle_get'),
	_ = crossbar_bindings:bind(<<"*.execute.post.auth">>, ?MODULE, 'handle_post').


% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
allowed_methods() ->
	[<<"GET">>, <<"POST">>].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) -> true.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(_Context) ->
	true.

authorize(_Context, _Id) ->
	true.

-spec validate(cb_context:context()) ->  cb_context:context().
validate(Context) ->
	validate_request(Context).

-spec handle_post(cb_context:context()) -> cb_context:context().
handle_post(Context) ->
	Body = cb_context:req_json(Context),
	%lager:info("auth_token: Auth Body: ~p ~n",[Body]),
	case lists:max([ wh_json:get_value(K, Body)
									 || K <- [<<"grant_type">>, <<"response_type">>]]) of
		<<"password">> ->
			lager:info("auth_token: password: ~n",[]),
			process_password_grant(Context, Body);
		<<"client_credentials">> ->
			process_client_credentials_grant(Context, Body);
		<<"token">> ->
			process_implicit_grant_stage2(Context, Body);
		<<"refresh_token">> ->
			process_refresh_token_grant(Context, Body);
		_ ->
			Msg =  <<"Bad Request.">>,
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, Msg},
													{fun cb_context:set_resp_error_code/2, 400}])
	end.


-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
	%%{ResponseType, Req2} = cowboy_req:qs_val(<<"response_type">>, Req),
	QueryJson = cb_context:query_string(Context),
	ResponseType = wh_json:get_value(<<"response_type">>, QueryJson, <<>>),
	case ResponseType of
		<<"token">> ->
			Params = cb_context:req_json(Context),
			lager:info("Params: ~p ~n",[Params]),
			{Req, process_implicit_grant(Context, Params)};
		_ ->
			JSON = jsx:encode([{error, <<"unsupported_response_type">>}]),
			{Req, cb_context:setters(Context,
															 [{fun cb_context:set_resp_data/2, JSON}])}

	end.
%{halt, Reply, State}.

%%%===================================================================
%%% Grant type handlers
%%%===================================================================
%% Scope divide into two types :
%% ?USER_ROLE_CUSTOMER : Authenticate for end customer
%% ?USER_ROLE_USER : Authenticate for User , admin of a business owner
%% Default of Scope is ?USER_ROLE_CUSTOMER
process_password_grant(Context, Params) ->
	Username =  wh_json:get_value(<<"username">>, Params, <<>>),
	Password =  wh_json:get_value(<<"password">>, Params, <<>>),
	Scope    =  wh_json:get_value(<<"scope">>, Params, ?USER_ROLE_CUSTOMER),
	%---Device---%
	DeviceInfo = wh_json:get_value(<<"device_info">>, Params, []),
	%lager:info("auth_token: DeviceInfo ~p ~n", [ReqDeviceInfo]),
	Auth = oauth2:authorize_password({Username, Password}, <<>>, Scope, [{scope, Scope}, {device_info, DeviceInfo}]),
	lager:info("auth_token: Auth ~p ~n", [Auth]),
	issue_token(Auth, Context).

process_refresh_token_grant(Context, Params) ->
	RefreshToken = wh_json:get_value(<<"refresh_token">>, Params, <<>>),
	Scope    =  wh_json:get_value(<<"scope">>, Params, ?USER_ROLE_CUSTOMER),
	Auth = oauth2:refresh_access_token(<<>>, RefreshToken, Scope, wh_json:to_proplist(Params)),
	emit_response(Auth, Context).


process_client_credentials_grant(Context, Params) ->
	Headers = cb_context:req_headers(Context),
	lager:info("header: ~p ~n",[Headers]),
	[Id, Secret] =
	case proplists:get_value(<<"authorization">>, Headers, <<>>) of
		<<"Basic ", Credentials/binary>>  ->
			lager:info("Credentials: ~p ~n; Base64: ~p ~n",[Credentials, base64:decode(Credentials)]),
			binary:split(base64:decode(Credentials), <<":">>);
		_ ->
			[<<>>, <<>>]
	end,
	lager:info("Id, Secret: ~p ~n",[Id, Secret]),
	Scope = wh_json:get_value(<<"scope">>, Params, <<>>),
	Auth = oauth2:authorize_client_credentials({Id, Secret}, Scope, []),
	issue_token(Auth, Context).

process_implicit_grant(Context, Params) ->
	State       = wh_json:get_value(<<"state">>, Params, <<>>),
	Scope       = wh_json:get_value(<<"scope">>, Params, <<>>),
	ClientId    = wh_json:get_value(<<"client_id">>, Params, <<>>),
	RedirectUri = wh_json:get_value(<<"redirect_uri">>, Params, <<>>),
	lager:info("State: ~p; Scope: ~p ; ClientId: ~p ;  RedirectUri: ~p ~n",[State, Scope, ClientId, RedirectUri]),
	case oauth2_app_backend:verify_redirection_uri(ClientId, RedirectUri,[]) of
		{ok, _} ->
			%% Pass the scope, state and redirect URI to the browser
			%% as hidden form parameters, allowing them to "propagate"
			%% to the next stage.
			{ok, Html} = auth_form:render([{redirect_uri, RedirectUri},
																		 {client_id, ClientId},
																		 {state, State},
																		 {scope, Scope}]),
			lager:info("oauth2:  Html: ~p ~n",[Html]),
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_data/2,  Html},
													{fun cb_context:set_resp_status/2, 'success'}]);

		%% TODO: Return an OAuth2 response code here.
		%% The returned Reason might not be valid in an OAuth2 context.
		{error, Reason} ->
			redirect_resp(RedirectUri,
										[{<<"error">>, to_binary(Reason)},
										 {<<"state">>, State}],
										Context)
	end.

process_implicit_grant_stage2(Context, Params) ->
	ClientId    = wh_json:get_value(<<"client_id">>, Params, <<>>),
	RedirectUri = wh_json:get_value(<<"redirect_uri">>, Params, <<>>),
	Username    = wh_json:get_value(<<"username">>, Params, <<>>),
	Password    = wh_json:get_value(<<"password">>, Params, <<>>),
	State       = wh_json:get_value(<<"state">>, Params, <<>>),
	Scope       = wh_json:get_value(<<"scope">>, Params, <<>>),
	lager:info("process_implicit_grant_stage2: ~p~n",[{ClientId, RedirectUri,Username, Password,  Scope}]),
	case oauth2:authorize_code_request({Username, Password}, ClientId, RedirectUri, Scope, []) of
		{ok, {_,AuthRes}} ->
			case oauth2:issue_code(AuthRes, []) of
				{ok, {_, Response}} ->
					Props = [{<<"state">>, State} |  oauth2_response:to_proplist(Response)],
					lager:info("Response: ~p ; ~p  ~n",[AuthRes, Props]),
					redirect_resp(RedirectUri, Props, Context);
				{error, Reason} ->
					redirect_resp(RedirectUri, [{<<"error">>, to_binary(Reason)},
																			{<<"state">>, State}], Context)
			end;
		{error, Reason} ->
			redirect_resp(RedirectUri, [{<<"error">>, to_binary(Reason)},
																	{<<"state">>, State}], Context)
	end.


validate_request(Context) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson = cb_context:req_json(Context),
	GrantType = wh_json:get_value(<<"grant_type">>, ReqJson, <<>>),
	ValidateFuns =
	case GrantType of
		<<"password">> ->

			[fun  validate_username/2
			 ,fun validate_password/2 ];

		<<"client_credentials">> ->

			[];

		_ ->
			[fun validate_grant_type/2]

	end,
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns).

validate_username(ReqJson, Context) ->
	Username = wh_json:get_value(<<"username">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"username">>, Username).

validate_password(ReqJson, Context) ->
	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"password">>, Password).

validate_grant_type(ReqJson, Context) ->
	GrantType = wh_json:get_value(<<"grant_type">>, ReqJson, <<>>),
	case lists:member(GrantType, ?GRANT_TYPE) of
		true ->
			Context;
		_ ->
			api_util:validate_error(Context, <<"grant_type">>, <<"invalid">>, <<"Field 'grant_type' must be password/refresh_token/client_credentials/token.">>)
	end.



%%%===================================================================
%%% Internal functions
%%%===================================================================
issue_token({ok, {{error, inactive}, _Auth}}, Context) ->
	cb_context:setters(Context,
										 [{fun cb_context:set_resp_data/2, <<"Inactive Account">>},
											{fun cb_context:set_resp_error_msg/2, <<"access_denied">>},
											{fun cb_context:set_resp_status/2, <<"error">>},
											{fun cb_context:set_resp_error_code/2, 401}
										 ]);

issue_token({ok, {{error, notfound}, _Auth}}, Context) ->
	cb_context:setters(Context,
										 [{fun cb_context:set_resp_data/2, <<"Not Found">>},
											{fun cb_context:set_resp_error_msg/2, <<"access_denied">>},
											{fun cb_context:set_resp_status/2, <<"error">>},
											{fun cb_context:set_resp_error_code/2, 401}
										 ]);

issue_token({ok, {Ctx,Auth}}, Context) ->
	% emit_response(oauth2:issue_token(Auth, Ctx), Context);
	emit_response(oauth2:issue_token_and_refresh(Auth, Ctx), Context);

issue_token(Error, Context) ->
	emit_response(Error, Context).

emit_response(AuthResult, Context) ->
	%lager:info("auth_token: AuthResult ~p ~n", [AuthResult]),
	{Status, Code, Resp, Msg} =
	case AuthResult of
		{error, Reason} ->
			{'error', 400, [], to_binary(Reason)};

		{ok,{Ctx,Response}} ->
			Res = oauth2_response:to_proplist(Response) ++ Ctx,
			{'success', 200, Res, <<>>}
	end,
	%lager:info("auth_token: Resp ~p ~n", [Resp]),
	AuthToken = proplists:get_value(<<"access_token">>, Resp),
	ReqDeviceInfo = proplists:get_value(<<"device_info">>, Resp, []),
	Scope = proplists:get_value(<<"scope">>, Resp, <<>>),
	UserId = proplists:get_value(<<"user_id">>, Resp, <<>>),
	ReqDeviceId = proplists:get_value(<<"device_id">>, ReqDeviceInfo, <<>>),
	OwnerInfo = [{<<"account_id">>, UserId},{<<"account_scope">>, Scope}],
	DeviceInfo = lists:merge(ReqDeviceInfo, OwnerInfo),
	DeviceId =	cb_device:add_device(ReqDeviceId, {DeviceInfo}),
	lager:info("auth_token: DeviceInfo ~p DeviceId ~p ~n", [DeviceInfo, DeviceId]),
	try
		Context2 =
		cb_context:setters(Context
											 ,[{fun cb_context:set_resp_error_msg/2, Msg}
												 ,{fun cb_context:set_resp_status/2, Status}
												 ,{fun cb_context:set_resp_error_code/2, Code}
												 ,{fun cb_context:set_resp_data/2, proplists:delete(<<"access_token">>, Resp)}
												 ,{fun cb_context:set_auth_token/2, AuthToken}
												]),


		lager:debug("Context2: ~p~n",[Context2]),
		Context2
	catch
		E1:E2 ->
			lager:error("E1: ~p, E2: ~p~n",[E1, E2])
	end.

to_binary(Atom) when is_atom(Atom) ->
	list_to_binary(atom_to_list(Atom)).

redirect_resp(RedirectUri, FragParams, Context) ->
	Frag = proplists:substitute_aliases([{<<"access_code">>, <<"auth_token">>}], FragParams),
	AuthToken = proplists:get_value(<<"auth_token">>, Frag),
	Context1 = cb_context:add_resp_header(Context, <<"Location">>, RedirectUri),
	lager:info("add response header: ~p ~n",[Context1]),
	cb_context:setters(Context1,
										 [{fun cb_context:set_resp_data/2, Frag},
											{fun cb_context:set_resp_error_msg/2, <<"redirect">>},
											{fun cb_context:set_auth_token/2, AuthToken},
											{fun cb_context:set_resp_error_code/2, 302}
										 ]).


errors() -> 
	Path = <<"auth">>,
	HandlePostValidates =
		[
			{<<"username">>, <<"required">>, <<"Field 'username' is required">>},
			{<<"password">>, <<"required">>, <<"Field 'password' is required">>},
			{<<"grant_type">>, <<"invalid">>, <<"Field 'grant_type' must be password/refresh_token/client_credentials/token.">>}
	 	],
	HandlePost = app_util:declare_api_validate(<<"post">>,Path,HandlePostValidates),
	Apis = [
			HandlePost
										  
	],
	app_util:create_module_validates(Apis).