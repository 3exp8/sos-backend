-module(authorize_util).

-include("crossbar.hrl").

-export([
         authorize/2,
         authorize/3,
         authorize_permission/3,
         authorize_permission/4,
         check_roles_permission/5,
         check_roles_permission/6,
         check_permission/7,
         authorize_ids/2,
         check_ids/2,
         permissions/0,
         permissions/1,
         default_permission/1,
         create_permission/2,
         get_role_permission/2,
         check_scopes/3
        ]).

-define(SCOPE_TYPE_ACCOUNT, <<"account">>).
-define(SCOPE_TYPE_BRANCH, <<"branch">>).
-define(SCOPE_TYPE_STORE, <<"store">>).

-define(AUTHORIZED_MODULES, [
                  {cb_accounts, {<<"user">>, ?SCOPE_TYPE_BRANCH}},
                  {cb_customers, {<<"customer">>, ?SCOPE_TYPE_BRANCH}},
                  {cb_role, {<<"role">>, ?SCOPE_TYPE_ACCOUNT}},
                  {cb_notification, {<<"notification">>, ?SCOPE_TYPE_BRANCH}}
    ]).

-define(AUTHORIZED_MODULE(Module), proplists:get_value(Module,?AUTHORIZED_MODULES, <<>>)).

-define(PERMISSION_ALL, <<"all">>).
-define(RES_PERMISSION_ALL, <<"_all">>).
-define(RES_PERMISSION_CREATE, <<"_create">>).
-define(RES_PERMISSION_UPDATE, <<"_update">>).
-define(RES_PERMISSION_GET_ALL, <<"_get_all">>).
-define(RES_PERMISSION_GET_DETAIL, <<"_get_detail">>).
-define(RES_PERMISSION_DELETE, <<"_delete">>).

-define(DEFAULT_PERMISSIONS, [
    {?RES_PERMISSION_ALL, <<"with all permissions">>}, 
    {?RES_PERMISSION_CREATE,<<"with create permission">>},
    {?RES_PERMISSION_UPDATE, <<"with update permission">>},
    {?RES_PERMISSION_DELETE,<<"with delete permission">>},
    {?RES_PERMISSION_GET_ALL,<<"with get list permission">>},
    {?RES_PERMISSION_GET_DETAIL, <<"get detail permission">>}
  ]).

authorize(Module, Context) ->
    lager:debug("authorize mod: ~p~n",[Module]),
    Verb = cb_context:req_verb(Context),
    AuthMod = ?AUTHORIZED_MODULE(Module),
    authorize_verb(AuthMod, Context, Verb).


authorize(Module, Context, {permission, _Permission} = Path) ->
    AuthMod = ?AUTHORIZED_MODULE(Module),
    authorize_permission(Context, AuthMod, Path);

authorize(Module, Context, Path) ->
    Verb = cb_context:req_verb(Context),
    AuthMod = ?AUTHORIZED_MODULE(Module),
    authorize_verb(AuthMod, Context, Verb, Path).

authorize_ids(Module, Context) ->
   AuthMod = ?AUTHORIZED_MODULE(Module),
   MyRoles = cb_context:roles(Context),
   check_ids(AuthMod, MyRoles).


permissions() ->
   
  ModulePermissons = permissions(?AUTHORIZED_MODULES),
  BasePermissions = base_permissions(),
  BasePermissions ++ ModulePermissons.


permissions(Module) when is_atom(Module) ->
  permissions([{Module,?AUTHORIZED_MODULE(Module)}]);

permissions([<<>>]) -> [];

permissions(Modules) when is_list(Modules) ->
  lists:map(fun({Module, {Prefix, _}}) -> 
    #{
      name => Prefix,
      permissions => Module:permissions()
    }
  end, Modules).

base_permissions() ->
  [
  #{
      name => <<"all">>,
      permissions => [#{
        name => <<"all">>,
        description => <<"all permission">>
      }]
  },
  #{
      name => <<"dashboard">>,
      permissions => [
      #{
        name => <<"dashboard_account">>,
        description => <<"Dashboard for account">>
      }
      ]
  }
  ].

default_permission(Module) ->
   case ?AUTHORIZED_MODULE(Module) of 
    <<>> -> [];
    {PermissionPrefix, _} -> 
      lists:map(fun({Permission, PermissionDescription}) ->
         #{
            name => <<PermissionPrefix/binary, Permission/binary>>,
            description => <<PermissionPrefix/binary," ",PermissionDescription/binary>>
         }
      end, ?DEFAULT_PERMISSIONS)
    end.


check_ids(<<>>, _) -> all;

check_ids({AuthMod, AuthType}, Roles) ->
  Ids =
  lists:filtermap(fun(#{<<"related_type">> := RelatedType, <<"related_id">> := RelatedId} = RoleInfo) -> 
   get_ids_by_type(RelatedType, AuthType,  RelatedId)
  end, Roles),
  case lists:member(all, Ids) of
           true ->  all;
           _ -> Ids
  end.

get_ids_by_type(<<"all">>, _AuthType, RelatedId) -> get_ids(RelatedId);
get_ids_by_type(AuthType, AuthType, RelatedId) -> get_ids(RelatedId);
get_ids_by_type(_AuthType1, _AuthType2, _AuthId) -> false.

get_ids(<<"all">>) -> {true, all};
get_ids(Id) -> {true, Id}.

authorize_verb(<<>>, _, _) -> true;

%authorize_verb({AuthMod, AuthType}, Context, ?HTTP_GET = Verb) when is_binary(AuthMod) ->
authorize_verb({AuthMod, AuthType}, Context, Verb) when is_binary(AuthMod) ->
    lager:debug("authorize_verb/3 AuthoMod: ~p, ",[AuthMod]),
    Permission = create_permission(AuthMod, Verb),
    authorize_permission(Context, {AuthMod, AuthType}, {permission, Permission}); 

% authorize_verb({AuthMod, AuthType}, Context, ?HTTP_PUT)  when is_binary(AuthMod) ->
%     Permission = <<AuthMod/binary, ?RES_PERMISSION_CREATE/binary>>,
%     authorize_permission(Context, {AuthMod, AuthType}, {permission, Permission});  

authorize_verb(_, _, _) -> false.

authorize_verb(<<>>, _Context, _Method, _Id) -> true;

%authorize_verb({AuthMod, AuthType}, Context, ?HTTP_GET, Id)  when is_binary(AuthMod) ->
authorize_verb({AuthMod, AuthType}, Context, Verb, Id)  when is_binary(AuthMod) ->
    %Permission = <<AuthMod/binary, ?RES_PERMISSION_GET_DETAIL/binary>>,
    Permission = create_permission(AuthMod, {Verb, Id}),
    authorize_permission(Context, {AuthMod, AuthType}, Id, {permission, Permission}); 

% authorize_verb({AuthMod, AuthType}, Context, ?HTTP_POST, Id)  when is_binary(AuthMod) ->
%     Permission = <<AuthMod/binary, ?RES_PERMISSION_UPDATE/binary>>,
%     authorize_permission(Context, {AuthMod, AuthType}, Id, {permission, Permission});

% authorize_verb({AuthMod, AuthType}, Context, ?HTTP_DELETE, Id)  when is_binary(AuthMod) ->
%   Permission = <<AuthMod/binary, ?RES_PERMISSION_DELETE/binary>>,
%   authorize_permission(Context, {AuthMod, AuthType},  Id, {permission, Permission});

authorize_verb(_, _, _, _) -> false.

authorize_permission(Context, {AuthMod, AuthType}, {permission, Permission}) ->
  PermissionAll = <<AuthMod/binary, ?RES_PERMISSION_ALL/binary>>,
  authorize_permission(Context, AuthType, [PermissionAll, Permission]);

authorize_permission(Context, AuthType, PermissionsList) ->
  lager:debug("authorize_permission/3 PermissionsList: ~p~n",[PermissionsList]),
  MyRoles = cb_context:roles(Context),
  AccountId = cb_context:account_id(Context),
  Scopes = cb_context:scopes(Context),
  lager:debug("authorize_permission check scope: ~p ~n",[Scopes]),
  check_roles_permission(AccountId, Scopes, MyRoles, AuthType, PermissionsList).

authorize_permission(Context, {AuthMod, AuthType},  Path, {permission, Permission}) ->
  PermissionAll = <<AuthMod/binary, ?RES_PERMISSION_ALL/binary>>,
  authorize_permission(Context, AuthType, Path, [PermissionAll, Permission]);

authorize_permission(Context, AuthType, Path, PermissionsList) ->
  MyRoles = cb_context:roles(Context),
  AccountId = cb_context:account_id(Context),
  Scopes = cb_context:scopes(Context),
  lager:debug("authorize_permission check scope: ~p ~n",[Scopes]),
  check_roles_permission(AccountId, Scopes, MyRoles, AuthType, Path, PermissionsList).

%[#{<<"related_id">> => <<"all">>,<<"related_type">> => <<"all">>,<<"roles">> => [<<"admin">>]}]

check_roles_permission(AccountId, Scopes, MyRoles, AuthType, PermissionsList) ->
    
    lists:any(fun(MyRoleInfo) -> 
      check_by_auth_type(AccountId, Scopes, AuthType, MyRoleInfo, <<>>, PermissionsList)
    end, MyRoles).

check_roles_permission(AccountId, Scopes, MyRoles, AuthType, Path, PermissionsList) ->
    lists:any(fun(MyRoleInfo) -> 
      check_by_auth_type(AccountId, Scopes, AuthType, MyRoleInfo, Path, PermissionsList)
    end, MyRoles).

% related_type: authorize by types
% related_id: authorize by id
check_by_auth_type(AccountId, Scopes, _ModAuthType, #{
    <<"related_id">> := UserScopeId, 
    <<"related_type">> := UserScopeType, 
    <<"roles">> := RolesList
  }, Path, PermissionsList) ->
    case check_scopes(Scopes, UserScopeType, UserScopeId) of 
      true ->     
          lists:any(fun(RoleName) -> 
            check_permission(AccountId, Scopes, RoleName, UserScopeType, UserScopeId, Path, PermissionsList)
          end, RolesList);
      _ -> false
    end;

% check_by_auth_type(ModAuthType, #{<<"related_type">> := ModAuthType, <<"related_id">> := RoleId, <<"roles">> := RolesList}, Id, PermissionsList) ->
%   lager:debug("check_by_auth_type ModAuthType: ~p, RoleId: ~p, RolesList: ~p, Id ~p, Permisison: ~p",[ModAuthType, RoleId, RolesList, Id, PermissionsList]),
%   lists:any(fun(RoleName) -> 
%     check_permission(RoleName, RoleId, Id, PermissionsList)
%   end, RolesList);

check_by_auth_type(_AccountId, _Scopes, _ModAuthType, _MyRoleInfo, _Path, _PermissionsList) -> false.

check_scopes([], _UserScopeType, _UserScopeId) -> true;

check_scopes(Scopes, UserScopeType, UserScopeId) ->
  lists:any(fun({ScopeType, ScopeId}) -> 
    Res = check_scope(ScopeType, ScopeId, UserScopeType, UserScopeId),
    lager:debug("check_scope Input: ~p result: ~p ~n",[{ScopeType, ScopeId, UserScopeType, UserScopeId}, Res]),
    Res
  end, Scopes).

check_scope(ScopeType, ScopeId, ScopeType, ScopeId) -> true;

check_scope(ScopeType, _ScopeId, <<"all">>, <<"all">>) -> true;
check_scope(ScopeType, _ScopeId, ScopeType, <<"all">>) -> true;

check_scope(ScopeType, ScopeId, UserScopeType, UserScopeId) when is_atom(ScopeType) ->
  ScopeTypeBin = zt_util:to_bin(ScopeType),
  ScopeIdBin = zt_util:to_bin(ScopeId),
  check_scope(ScopeTypeBin, ScopeIdBin, UserScopeType, UserScopeId);


check_scope(_ScopeType, _ScopeId, _UserScopeType, _UserScopeId) -> false.
  


%-spec check_permission(AccountId, , binary(), binary(), list()) -> boolean().
%-spec check_permission(binary(), binary(), binary(), binary(), binary(), list()) -> boolean().
% check_permission(AccountId, Scopes, RoleName, _UserScopeType, <<"all">>, _Path, PermissionsList) -> 
%     check_permission(AccountId, RoleName, PermissionsList);

% check_permission(AccountId, Scopes, RoleName, _, <<"all">>, PermissionsList) -> 
%     check_permission(AccountId, RoleName, PermissionsList);

check_permission(AccountId, _Scopes, RoleName, _UserScopeType, _UserScopeId, _Path, PermissionsList) -> 
    check_permission(AccountId, RoleName, PermissionsList);

check_permission(_AccountId, _Scopes, _RoleName, _UserScopeType, _UserScopeId, _Path, _PermissionsList) -> false.


check_permission(AccountId, RoleName, Permission) when is_binary(Permission) ->
  check_permission(AccountId, RoleName, [Permission]);

check_permission(AccountId, RoleName, PermissionsList) ->
  AllPermissions = [?PERMISSION_ALL | PermissionsList],
  case role_db:find_by_conditions([{account_id, AccountId},{name, RoleName}],[],2,0) of
      [] ->  false;
      [RoleInfo] when is_map(RoleInfo) ->
        RolePermissionList = maps:get(permissions, RoleInfo, []),
        lists:any(fun(Permission) -> 
         lists:member(Permission, RolePermissionList)
        end, AllPermissions);
      _ -> false
    end.

get_role_permission(AccountId, RoleName) ->
  case role_db:find_by_conditions([{account_id, AccountId},{name, RoleName}],[],2,0) of
      [RoleInfo] when is_map(RoleInfo) ->
        maps:get(permissions, RoleInfo, []);
      _ -> []
    end.


create_permission(Module, Verb) when is_atom(Module) ->
    {AuthMod,_} = ?AUTHORIZED_MODULE(Module),
    create_permission(AuthMod, Verb);

create_permission(AuthMod, ?HTTP_PUT) ->
  <<AuthMod/binary,?RES_PERMISSION_CREATE/binary>>;

create_permission(AuthMod, ?HTTP_GET) ->
  <<AuthMod/binary,?RES_PERMISSION_GET_ALL/binary>>;

create_permission(AuthMod, {?HTTP_GET, Id}) ->
  <<AuthMod/binary,?RES_PERMISSION_GET_DETAIL/binary>>;

create_permission(AuthMod, {?HTTP_POST, Id}) ->
  <<AuthMod/binary,?RES_PERMISSION_UPDATE/binary>>;

create_permission(AuthMod, {?HTTP_DELETE, Id}) ->
  <<AuthMod/binary,?RES_PERMISSION_DELETE/binary>>;

create_permission(Module, _) ->  
  <<"unknown">>.