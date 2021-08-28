-module(cb_role).

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
	,handle_put/1
  ,handle_post/2
  ,handle_delete/2
	]).

-export([
          permissions/0
  ]).

init() ->
		_ = crossbar_bindings:bind(<<"*.resource_exists.roles">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.roles">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authenticate.roles">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.roles">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.roles">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.roles">>, ?MODULE, 'handle_get'),
    _ = crossbar_bindings:bind(<<"*.execute.post.roles">>, ?MODULE, 'handle_post'),
		_ = crossbar_bindings:bind(<<"*.execute.put.roles">>, ?MODULE, 'handle_put'),
		_ = crossbar_bindings:bind(<<"*.execute.delete.roles">>, ?MODULE, 'handle_delete').


-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().

%% /api/v1/roles
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

allowed_methods(_RoleId) ->
	[?HTTP_DELETE, ?HTTP_POST, ?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.

resource_exists() -> 'true'.

resource_exists(_RoleId) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _PermissionId) ->
  Token = cb_context:auth_token(Context),
  app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context) ->
    authorize_util:authorize(?MODULE, Context).

authorize(Context, Id) ->
    authorize_util:authorize(?MODULE, Context, Id). 

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().

validate(Context) ->
  validate_request(Context, cb_context:req_verb(Context)).

validate(Context, RoleId) ->
  validate_request(RoleId, Context, cb_context:req_verb(Context)).

-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
	QueryJson = cb_context:query_string(Context),
  RequesterId = cb_context:user_id(Context),
  AccountId = cb_context:account_id(Context),
  Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
  PropQueryJson = wh_json:to_proplist(QueryJson),
  Permissions =  role_db:find_by_conditions([{account_id, AccountId}], PropQueryJson, Limit, Offset),
  {Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, Permissions}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.


-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, RoleId) ->

  case role_db:find(RoleId) of
    RoleInfo when is_map(RoleInfo) ->
          {Req, cb_context:setters(Context
                                   ,[{fun cb_context:set_resp_data/2, RoleInfo}
                                     ,{fun cb_context:set_resp_status/2, 'success'}
                                     ])};

    notfound ->
      {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, ?HTTP_MSG_NOT_FOUND},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_NOT_FOUND}])};

    Error ->
       {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, ?HTTP_MSG_SERVER_ERROR},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_SERVER_ERROR}])}
  end.

-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->

    ReqJson = cb_context:req_json(Context),
    Name = zt_util:to_lower(wh_json:get_value(<<"name">>, ReqJson, <<>>)),
    case role_db:find_by_name(Name) of
      [] ->
          Description  = wh_json:get_value(<<"description">>, ReqJson),
          Permissions = lists:map(fun(Permission) -> 
            zt_util:to_lower(Permission)  
          end, wh_json:get_value(<<"permissions">>, ReqJson, [])),
          
          Id = zt_util:get_uuid(),
          RoleInfo =  #{
            id => Id,
            account_id => cb_context:account_id(Context),
            name => Name, 
            permissions => Permissions, 
            description => Description,
            created_by => cb_context:user_id(Context),
            created_time => zt_datetime:get_now()
         },
          role_db:save(RoleInfo),
        	cb_context:setters(Context
            ,[{fun cb_context:set_resp_data/2, RoleInfo}
             ,{fun cb_context:set_resp_status/2, 'success'}]);

        RoleList when length(RoleList) >0 -> 
            
            cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, <<"Role existed!">>},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_BAD_REQUEST}]) ;
        Error -> 
            lager:error("Error when check role: ~p, Error: ~p~n",[Name, Error]),
            cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, ?HTTP_MSG_SERVER_ERROR},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_SERVER_ERROR}])
        end.



-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, RoleId) ->
  ReqJson =  cb_context:req_json(Context),
  case role_db:find(RoleId) of
    #{description := DescriptionDb, permissions := PermissionsDb} = RoleInfo when is_map(RoleInfo) ->
          Description = wh_json:get_value(<<"description">>, ReqJson, DescriptionDb),
          %Permissions = api_util:uppercase_all(wh_json:get_value(<<"permissions">>, ReqJson, [])),
          
          Permissions = lists:map(fun(Permission) -> 
            zt_util:to_lower(Permission)  
          end, wh_json:get_value(<<"permissions">>, ReqJson, PermissionsDb)),
          lager:debug("update id: ~p Permissions: ~p~n",[RoleId, Permissions]),
          NewRoleInfo1 = maps:merge(RoleInfo, #{
            permissions => Permissions,
            description => Description,
            updated_by => cb_context:user_id(Context),
            updated_time => zt_util:now_to_utc_binary(os:timestamp())
          }),
          NewRoleInfo = 
          case wh_json:get_value(<<"name">>, ReqJson, <<>>) of 
            <<>> -> NewRoleInfo1;
            NewName -> 
              NewNameLower = zt_util:to_lower(NewName),
              case user_db:find_by_conditions([{<<"roles#roles">>,NewNameLower}],[],1,0) of 
                [] -> 
                  maps:merge(NewRoleInfo1, #{
                    name => NewNameLower
                  });
                _ -> NewRoleInfo1
              end
          end,

          role_db:save(NewRoleInfo),
          cb_context:setters(Context, [{fun cb_context:set_resp_data/2, NewRoleInfo}
                                       ,{fun cb_context:set_resp_status/2, 'success'}]);
    notfound ->
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, ?HTTP_MSG_NOT_FOUND},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_NOT_FOUND}]
      );
    Error ->
      lager:error("get role error. RoleId: ~p, Error: ~p~n",[RoleId, Error]),
      cb_context:setters(Context,
                         [{fun cb_context:set_resp_error_msg/2, ?HTTP_MSG_SERVER_ERROR},
                          {fun cb_context:set_resp_status/2, <<"error">>},
                          {fun cb_context:set_resp_error_code/2, ?HTTP_CODE_SERVER_ERROR}])
  end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->

    RespData = role_db:del_by_id(Id),
    lager:info("delete ", RespData),
	  cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
										,{fun cb_context:set_resp_status/2, 'success'}
										]).

permissions() ->
  authorize_util:default_permission(?MODULE).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().

%% PUT api/v1/roles

validate_request(Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]) ;

validate_request(Context, ?HTTP_PUT) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),
  ValidateFuns = [
    fun validate_name/2,
    fun validate_permission/2
                 ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(Context, _Verb) ->
  Context.


validate_request(_RoleId, Context, ?HTTP_GET) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_RoleId, Context, ?HTTP_POST) ->
  ReqJson = cb_context:req_json(Context),
  Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),

  ValidateFuns = [ ],
  lists:foldl(fun(F, C) ->
                  F(ReqJson, C)
              end, Context1,  ValidateFuns);

validate_request(_RoleId, Context, ?HTTP_DELETE) ->
  cb_context:setters(Context
                     ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_RoleId, Context, _Verb) ->
  Context.

validate_name(ReqJson, Context) ->
	Name = wh_json:get_value(<<"name">>, ReqJson, <<>>),
	case api_util:check_val(Context, <<"name">>, Name) of 
    Context -> 
      validate_name_exist(ReqJson, Context);
    NewContext -> NewContext
  end.


validate_name_exist(ReqJson, Context) ->
    AccountId = cb_context:account_id(Context),
    Name = wh_json:get_value(<<"name">>, ReqJson, <<>>),
    case role_db:find_by_conditions([{account_id, AccountId}, {name, Name}], [], 1, 0) of 
          [] -> Context;
          _ ->
            api_util:validate_error(Context, <<"bane">>, <<"invalid">>, <<"Role name is alway existed!">>)
    end.


validate_permission(ReqJson, Context) ->
  case wh_json:get_value(<<"permissions">>, ReqJson, <<>>) of
    Permissions when is_list(Permissions) -> 
      Context;
    _ ->
      api_util:validate_error(Context, <<"permissions">>, <<"invalid">>, <<"Permission is not valid">>)
    end.