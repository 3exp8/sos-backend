-module(cb_permission).

-include("crossbar.hrl").

-export([init/0
	,validate/1
  ,validate/2
	,resource_exists/0
  ,resource_exists/1
	,authenticate/1
  ,authenticate/2
  ,authorize/1
  ,authorize/2
	,allowed_methods/0
  ,allowed_methods/1
	,handle_get/1
  ,handle_get/2
]).

-export([
          permissions/0,
          get_permissions/2,
          get_permissions_related_type/6
  ]).

-define(PATH_CHECK, <<"check">>).

init() ->
		_ = crossbar_bindings:bind(<<"*.resource_exists.permissions">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.permissions">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authenticate.permissions">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.permissions">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.permissions">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.permissions">>, ?MODULE, 'handle_get').

-spec allowed_methods() -> http_methods().

%% /api/v1/permission
allowed_methods() ->
	[?HTTP_GET].

allowed_methods(?PATH_CHECK) ->
  [?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.

resource_exists(?PATH_CHECK) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  lager:debug("permission authenticate~n",[]),
	true.

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, ?PATH_CHECK) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context) ->
    authorize_util:authorize(?MODULE, Context).

authorize(Context, ?PATH_CHECK) ->
    true. 

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().

validate(Context) ->
  validate_request(Context, cb_context:req_verb(Context)).

validate(Context, ?PATH_CHECK = Path) ->
  validate_request(Path, Context, cb_context:req_verb(Context)).

-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
  Permissions =  authorize_util:permissions(),
  {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, Permissions}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.

-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, ?PATH_CHECK) ->
  QueryJson = cb_context:query_string(Context),
  ReqRelatedType = wh_json:get_value(<<"related_type">>, QueryJson, <<>>),
  ReqRelatedId = wh_json:get_value(<<"related_id">>, QueryJson, <<>>),
  Roles = cb_context:roles(Context),
  AccountId = cb_context:account_id(Context),
  
  % roles => [#{<<"related_id">> => <<"all">>,
  %     <<"related_type">> => <<"all">>,
  %     <<"roles">> => [<<"admin">>]}],
lager:debug(" check permission AccountId: ~p, Roles: ~p~n",[AccountId, Roles]),
Permissions = 
  lists:foldl(fun(#{
      <<"related_id">> := RelatedId,
      <<"related_type">> := RelatedType,
      <<"roles">> := SubRoles  
    } = RoleInfo, PermissionAcc) -> 
    PermissionAcc ++ get_permissions_related_type(AccountId, SubRoles, ReqRelatedType, ReqRelatedId, RelatedType, RelatedId)
  end, [], Roles),
lager:debug(" check permission Permissions: ~p~n",[Permissions]),
  AllPermissions = authorize_util:permissions(),

  FilteredPermissions = 
  lists:filtermap(fun(#{permissions := ApiPermissions} = PermissionInfo) -> 
    case get_user_permission(ApiPermissions, Permissions) of 
      [] -> false;
      UserPermission -> 
        {true, maps:merge(PermissionInfo, #{
            permissions => UserPermission
          })}
    end
  end, AllPermissions),
lager:debug(" check permission FilteredPermissions: ~p~n",[FilteredPermissions]),
  {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, FilteredPermissions}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.

permissions() ->
  authorize_util:default_permission(?MODULE).

get_user_permission(ApiPermissions, UserPermissions) ->
  lists:filtermap(fun(#{name := PermissionName} = PermissionInfo) -> 
      case lists:member(PermissionName, UserPermissions) of 
        true -> {true, PermissionInfo};
        _ -> false
      end
  end, ApiPermissions).

get_permissions_related_type(AccountId, _Roles, <<>>, _ReqScopeId, _ScopeType, _ScopeId) -> [];

get_permissions_related_type(AccountId, Roles, ReqScopeType, ReqScopeId, <<"all">>, ScopeId) -> 
  get_permissions_related_id(AccountId, Roles, ReqScopeType, ReqScopeId, ScopeId);

get_permissions_related_type(AccountId, Roles, ReqReScopeType, ReqScopeId, ReqScopeType, ScopeId) -> 
  get_permissions_related_id(AccountId, Roles, ReqScopeType, ReqScopeId, ScopeId);

get_permissions_related_type(_AccountId, _Roles, _ReqScopeType, _ReqScopeId, _ScopeType, _ScopeId) -> [].

get_permissions_related_id(AccountId, Roles, ScopeType, ReqScopeId,  <<"all">>) -> get_permissions(AccountId, Roles);

get_permissions_related_id(AccountId, Roles, ScopeType, ReqScopeId,  ReqScopeId) -> 
  get_permissions(AccountId, Roles);

get_permissions_related_id(_, _, _, _,  _) -> 
  lager:debug("get_permissions_related_id other ~n",[]),
[].

get_permissions(AccountId, Roles) -> 
  lager:debug("get_permissions AccountId: ~p, roles: ~p~n",[AccountId, Roles]),
  RolesList = role_db:find_by_conditions([{account_id, AccountId}, {name, in, Roles}],[],100,0),
  lists:foldl(fun(#{permissions := Permissions} = RoleInfo, PermisisonAcc)-> 
    PermisisonAcc ++ Permissions
  end, [], RolesList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

%% PUT api/v1/permission

validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, _Verb) ->
  Context.

validate_request(?PATH_CHECK, Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(_, Context, _Verb) ->
  Context.  