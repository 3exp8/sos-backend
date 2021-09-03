-module(oauth2_app_backend).

-behavior(oauth2_backend).

-include("crossbar.hrl").

%%% API
-export([
		start/0
		,stop/0
		]).

%%% Behavior API
-export([authenticate_user/2]).
-export([authenticate_client/2]).
-export([get_client_identity/2]).
-export([associate_access_code/3]).
-export([associate_refresh_token/3]).
-export([associate_access_token/3]).
-export([resolve_access_code/2]).
-export([resolve_refresh_token/2]).
-export([resolve_access_token/2]).
-export([revoke_access_code/2]).
-export([revoke_access_token/2]).
-export([revoke_refresh_token/2]).
-export([get_redirection_uri/1]).
-export([verify_redirection_uri/3]).
-export([verify_client_scope/3]).
-export([verify_resowner_scope/3]).
-export([verify_scope/3]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
		ok.
stop() ->
		ok.

%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================
%% There are two kind of scope of authenticate 
%% First scope : ?USER_ROLE_CUSTOMER : Customer  
%% Second scope: ?USER_ROLE_USER : user  admin
%% Default scope : ?USER_ROLE_CUSTOMER
authenticate_user(User, Ctx) ->
	Scope = proplists:get_value(scope, Ctx, ?USER_ROLE_CUSTOMER),
	if Scope == ?USER_ROLE_USER; 
		Scope == ?USER_ROLE_ADMIN ->
		authenticate_scope_user(User, Ctx) ;
	true ->
		authenticate_scope_customer(User, Ctx)
	end. 

authenticate_scope_user({UserId, {confirm_code, ConfirmCode}}, Ctx) ->
	lager:debug("authenticate_scope_user: UserId: ~p, ConfirmCode: ~p, Ctx ~p ~n", [UserId, ConfirmCode, Ctx]),
	DeviceInfo = proplists:get_value(device_info, Ctx, []),
	case user_db:find(UserId) of
	#{
		confirm_code := ConfirmCode,
		account_id := AccountId,
		phone_number := Username,
		role := Role, 
		roles := Roles
	}  ->
		NewCtx = [
			{<<"user_id">>, UserId}, 
			{<<"account_id">>, AccountId}, 
			{<<"role">>, Role}, 
			{<<"roles">>, Roles}, 
			{<<"device_info">>, DeviceInfo}
		],
		lager:info("auth_token: NewCtx ~p ~n", [NewCtx]),
		{ok, {NewCtx, Username}};
	_ ->
		{ok, {{error, notfound}, UserId}}
	end;

authenticate_scope_user({Username, Password}, Ctx) ->
	%lager:info("auth_token: Ctx ~p ~n", [Ctx]),
	DeviceInfo = proplists:get_value(device_info, Ctx, []),
	case user_db:find_by_phone_number(Username) of
	[
	#{
		id := UserId, 
		account_id := AccountId, 
		password := SecretKey, 
		role := Role, 
		%roles := Roles, 
		status := Status
	}| _] when SecretKey /= <<>> ->
		PassHash = zt_util:to_str(SecretKey),
		{ok, ProvidedHash} = bcrypt:hashpw(Password, PassHash),
		case PassHash == ProvidedHash of 
		true -> 
			if 
				Status == ?USER_STATUS_ACTIVE ->
					lager:info("auth_token------------------emnvn authenticate_scope_user user_id: ~p, AccountId: ~p ~n",[UserId, AccountId]),
					% Scope = proplists:get_value(scope, Ctx, ?USER_ROLE_CUSTOMER),
					NewCtx = [
						{<<"user_id">>, UserId}, 
						%{<<"account_id">>, AccountId}, 
						{<<"role">>, Role}, 
						%{<<"roles">>, Roles}, 
						{<<"device_info">>, DeviceInfo}
					],
					lager:info("auth_token: NewCtx ~p ~n", [NewCtx]),
					{ok, {NewCtx, Username}};
				Status == ?USER_STATUS_INACTIVE ->
					{ok, {{error, inactive}, Username}};

				true ->
					{ok, {{error, notfound}, Username}}
			end ;
		_  ->
			{error, badpass}
		end;
	_ ->
		{ok, {{error, notfound}, Username}}
	end.


authenticate_scope_customer({CustomerId, PhoneNumber}, Ctx) ->
	lager:info("------------------emnvn In compare PIN CustomerId: ~p ~n",[CustomerId]),
	Scope = proplists:get_value(scope, Ctx, ?USER_ROLE_CUSTOMER),
	if Scope == ?USER_ROLE_CUSTOMER ->
		NewCtx = [{<<"customer_id">>, CustomerId}, {<<"role">>, ?USER_ROLE_CUSTOMER}],
		{ok, {NewCtx, PhoneNumber}};
	true -> {error, invalid_scope}
	end.



authenticate_client({ClientKey, _ClientSecret}, Ctx) ->
	{ok, {Ctx, ClientKey}};

authenticate_client(Client, Ctx) ->
	{ok, {Ctx, Client}}.

get_client_identity(ClientKey, _) ->
	case db:find_client(ClientKey) of 

		#{client_key := ClientKey} -> {ok, {<<"client">>, ClientKey}};

		notfound -> {error, notfound}
	end.

associate_access_code(AccessCode, Context, _AppContext) ->
	associate_access_token(AccessCode, Context, _AppContext).

associate_refresh_token(RefreshToken, Context, Ctx) ->
	lager:info("ReFresh Token: Ctx ~p ~n", [Ctx]),
	Role = proplists:get_value(<<"role">>, Ctx, ?USER_ROLE_CUSTOMER),
	AccessToken = proplists:get_value(<<"access_token">>, Ctx, <<>>),
	lager:info("TEST access token ~p ~n", [AccessToken]),
	Id = case Role of
		?USER_ROLE_CUSTOMER ->
				proplists:get_value(<<"customer_id">>, Ctx, <<>>);
		_ ->
			proplists:get_value(<<"account_id">>, Ctx, <<>>)
	end,
	MapContext= maps:from_list(Context),
	Info = #{
		token => RefreshToken,
		access_token => AccessToken,
		account_id => Id,
		context => MapContext,
		role => Role
	},
	refresh_token_db:save(Info),
	{ok, Ctx}.

associate_access_token(AccessToken, Context, Ctx) ->
	lager:info("Access Token: Ctx ~p ~n", [Ctx]),
	Role = proplists:get_value(<<"role">>, Ctx, ?USER_ROLE_USER),
	RefreshToken = proplists:get_value(<<"refresh_token">>, Ctx, <<>>),
	lager:info("TEST refresh token ~p ~n", [RefreshToken]),
	{UserId, AccountId} = 
	case Role  of
		?USER_ROLE_CUSTOMER ->
			{proplists:get_value(<<"customer_id">>, Ctx, <<>>), <<>>};
		_ ->
			{proplists:get_value(<<"user_id">>, Ctx, <<>>), proplists:get_value(<<"account_id">>, Ctx, <<>>)}
	end,
	MapContext= maps:from_list(Context),
	Info = #{
		token => AccessToken,
		refresh_token => RefreshToken,
		account_id => AccountId,
		user_id => UserId,
		context => MapContext,
		role => Role,
		roles => proplists:get_value(<<"roles">>, Ctx, [])
	},
	access_token_mnesia_db:save(Info),
	{ok, lists:merge(Ctx, [{<<"access_token">>, AccessToken}])}.

resolve_access_code(AccessCode, _AppContext) ->
	resolve_access_token(AccessCode, _AppContext).

resolve_refresh_token(RefreshToken, Ctx) ->
	case refresh_token_db:find(RefreshToken) of 
	#{account_id := Id, context := Context, role := Role} ->
		{RequesterId, NewCtx} = 
		if Role == ?USER_ROLE_CUSTOMER ->
			CustomerId = proplists:get_value(<<"customer_id">>, Ctx, <<>>),
			{CustomerId, [{<<"customer_id">>, CustomerId}]};
		true ->
			AccountId =  proplists:get_value(<<"account_id">>, Ctx, <<>>),
			{AccountId, [{<<"account_id">>, AccountId}]}
		end,
		if RequesterId == Id ->
			ListContext= lists:map(fun
				({expiry_time =K, V}) -> {to_binary(K), to_integer(V)} ;
				({K, V}) -> {to_binary(K), V}
			end, maps:to_list(Context)), 
			{ok, {[{<<"role">>, Role} | NewCtx], ListContext}};
		true -> {error, invalid}
		end;
	_ ->
		{error, invalid}
	end. 
	%resolve_access_token(RefreshToken, _AppContext).

resolve_access_token(AccessToken, Ctx) ->
	%% The case trickery is just here to make sure that
	%% we don't propagate errors that cannot be legally
	%% returned from this function according to the spec.
	%case db:find_by_token(AccessToken) of
	case access_token_mnesia_db:find_by_token(AccessToken) of
	#{context := Context, user_id := UserId, account_id := AccountId, roles := Roles} -> 
		ListContext= 
		lists:map(fun({expiry_time =K, V}) -> {to_binary(K), to_integer(V)} ;
					 ({K, V}) -> {to_binary(K), V}
		end, maps:to_list(Context)), 
		lager:debug("order_authorize  resolve_access_token Context: ~p~n",[Context]),
		Scope = maps:get(<<"scope">>, Context, <<>>),
		Role = 
		case Scope of 
			<<"CUSTOMER">> -> ?USER_ROLE_CUSTOMER;
			_ -> <<>>
		end,

		Resp = [{<<"user_id">>, UserId},{<<"account_id">>, AccountId}, {<<"role">>, Role}, {<<"roles">>, Roles} | ListContext],
		lager:debug("order_authorize resolve_access_token Resp: ~p~n",[Resp]),
		{ok, {Ctx, Resp}};
	_ -> {error, notfound}
	end.

revoke_access_code(AccessCode, _AppContext) ->
	revoke_access_token(AccessCode, _AppContext).

revoke_access_token(AccessToken, _) ->
	access_token_mnesia_db:del_by_token(AccessToken).

revoke_refresh_token(RefreshToken, _) ->
	refresh_token_db:del_by_token(RefreshToken).


get_redirection_uri(ClientKey) ->
	case db:find_client(ClientKey) of 
		#{redirect_uri := RedirectUri} -> {ok, RedirectUri};
		notfound -> {error, notfound}
	end.

verify_redirection_uri(ClientKey, ClientUri, Ctx) ->
	case get_redirection_uri(ClientKey) of
		{ok, RedirectUri} when ClientUri =:= RedirectUri ->
				{ok, Ctx};
		_ ->
				{error, mismatch}
	end.

verify_client_scope(_ClientId, Scope, Ctx) ->
		{ok, {Ctx, Scope}}.

verify_resowner_scope(_ResOwner, Scope, Ctx) ->
		{ok, {Ctx,Scope}}.

verify_scope(Scope, Scope, Ctx) ->
	{ok, {Ctx, Scope}};
verify_scope(_, _, _) ->
	{error, invalid_scope}.

to_binary(Val) when is_atom(Val) ->
	list_to_binary(atom_to_list(Val));

to_binary(Val) when is_list(Val) ->
	list_to_binary(Val);

to_binary(Val)  when is_integer(Val) ->
	list_to_binary(integer_to_list(Val));

to_binary(Val) ->
	Val.


to_integer(Val) when is_binary(Val) ->
	binary_to_integer(Val) ;

to_integer(Val) when is_list(Val) ->
	list_to_integer(Val) ;

to_integer(Val)  ->
	Val. 
