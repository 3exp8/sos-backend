-module(cb_support_type).
-include("crossbar.hrl").

-export([init/0,
         allowed_methods/0,
         allowed_methods/1,
         resource_exists/0,
         resource_exists/1,
         authenticate/1,
         authenticate/2,
         authorize/1,
         authorize/2,
         validate/1,
         validate/2
        ]).

-export([
    handle_get/1, 
    handle_get/2, 
    handle_put/1, 
    handle_post/2,
    handle_delete/2
]).

-export([
          permissions/0
  ]).

-define(PATH_SEARCH, <<"search">>).


init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.support_types">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.support_types">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.support_types">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.support_types">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.support_types">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.support_types">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.support_types">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.support_types">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.support_types">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.support_types">>, ?MODULE, handle_delete).


allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) -> [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].


-spec resource_exists() -> true.
resource_exists() -> true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Id) -> true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_verb(Context, cb_context:req_verb(Context)).

authenticate_verb(_Context, ?HTTP_GET)  -> true;

authenticate_verb(Context, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, Path) ->
    authenticate_verb(Context, Path, cb_context:req_verb(Context)).

authenticate_verb(_Context, _Path, ?HTTP_GET)  -> true;

authenticate_verb(Context, ?PATH_SEARCH, _) -> true;

authenticate_verb(Context, Path, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) -> true;

authorize_verb(Context, ?HTTP_PUT) ->
    Role = cb_context:role(Context),
    Role == ?USER_ROLE_USER.

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Path, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context, Path);

authorize_verb(Context, ?PATH_SEARCH, ?HTTP_POST) -> true;

authorize_verb(Context, Path, ?HTTP_POST) ->
    Role = cb_context:role(Context),
    Role == ?USER_ROLE_USER;

authorize_verb(Context, Path, ?HTTP_DELETE) ->
    Role = cb_context:role(Context),
    Role == ?USER_ROLE_USER.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).

validate(Context, Id, Path) ->
    validate_request(Id, Path, Context, cb_context:req_verb(Context)).

%%%%%%%%%%%%%%%%
%%  HANDLERS  %%
%%%%%%%%%%%%%%%%

-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
    lager:debug("PropQueryJson: ~p~n",[PropQueryJson]),
    SupportTypes = support_type_db:find_by_conditions([], PropQueryJson, Limit, Offset),
    PropSupportTypes = 
        lists:map(fun (Info) ->
            get_sub_fields(Info)
        end,SupportTypes),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, PropSupportTypes},
                         {fun cb_context:set_resp_status/2, success}])}.

 
handle_get({Req, Context}, ?PATH_SEARCH) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    UserGroupCond = 
        case wh_json:get_value(<<"group">>,QueryJson,<<>>) of 
            <<>> -> {<<"user">>, <<>>};
            Val -> {<<"group">>, Val}
        end,
    NewQueryJson = wh_json:delete_keys([<<"user">>,<<"group">>], QueryJson),
    QueryJsonList = wh_json:to_proplist(NewQueryJson),
    Conds = 
        lists:foldl(fun({Type,Value}, Acc) -> 
            TempCond = 
                case Type of 
                <<"user">> -> {<<"target_types#type">>, Type};
                _ -> 
                    {
                        'and', [
                            {<<"target_types#type">>, Type},
                            {<<"target_types#value">>, 'in', zt_util:split_string(Value)}
                        ]
                    }
                end,
                [TempCond|Acc]
        end,[],[UserGroupCond|QueryJsonList]),
    FinalConds = 
    case Conds of 
        [] -> [];
        _ -> [{'or',Conds}]
    end,
    Types = support_type_db:find_by_conditions(FinalConds,[{priority,asc}],Limit,Offset),
    TypesFormated = 
    lists:map(fun(TypeInfo) -> 
        get_sub_fields_search(TypeInfo) 
    end,Types),
    {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, TypesFormated},
                               {fun cb_context:set_resp_status/2, success}])};

handle_get({Req, Context}, Id) ->
    case support_type_db:find(Id) of
      #{} = Info ->
          PropSupportType = get_sub_fields(Info),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, PropSupportType},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Support Type not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end.

-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
    ReqJson = cb_context:req_json(Context),
    Uuid = zt_util:get_uuid(),    
    UserId = cb_context:user_id(Context),
    Info = #{
        id => <<"support_type", Uuid/binary>>,
        type => wh_json:get_value(<<"type">>, ReqJson,<<>>),
        name => wh_json:get_value(<<"name">>, ReqJson,<<>>),
        unit => wh_json:get_value(<<"unit">>, ReqJson,<<>>),
        target_types => zt_util:to_map_list(wh_json:get_value(<<"target_types">>, ReqJson,[])),
        created_time => zt_util:now_to_utc_binary(os:timestamp()),
        created_by_id => UserId
    },
    support_type_db:save(Info),
    cb_context:setters(Context,
                    [{fun cb_context:set_resp_data/2, Info},
                        {fun cb_context:set_resp_status/2, success}]).

-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, Id) ->
    case support_type_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Support Type Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            name := NameDb,
            unit := UnitDb,
            target_types := TargetTypesDb
        } = InfoDb -> 
         ReqJson =  cb_context:req_json(Context),
        NewTargetTypes = 
            case wh_json:get_value(<<"target_types">>, ReqJson, []) of
                [] ->  TargetTypesDb;
                TargetTypesProps -> zt_util:to_map_list(TargetTypesProps)
            end,

         NewInfo = 
            maps:merge(InfoDb, #{
                name => wh_json:get_value(<<"name">>, ReqJson,NameDb),
                unit => wh_json:get_value(<<"unit">>, ReqJson,UnitDb), 
                target_types => NewTargetTypes,
                updated_time => zt_util:now_to_utc_binary(os:timestamp()),
                updated_by_id => cb_context:user_id(Context)
            }),
            support_type_db:save(NewInfo),
         cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                             ])            
    end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    case support_type_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Support Type Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        _ -> 
            
            support_type_db:del_by_id(Id),
            Resp = #{
                id => Id
            },
            cb_context:setters(Context,
                       [{fun cb_context:set_resp_data/2, Resp},
                        {fun cb_context:set_resp_status/2, success}])
    end.

permissions() ->
  authorize_util:default_permission(?MODULE).
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(Context, ?HTTP_PUT = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun validate_type/2,
                    fun validate_name/2
                   % fun validate_detail_info/2
                ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Context, _Verb) ->
    Context.

validate_request(?PATH_SEARCH, Context, ?HTTP_GET = _Verb) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);


validate_request(_Id, Context, ?HTTP_POST = _Verb) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, _Verb) ->
    Context.

validate_request(_Id, _Path, Context, _Verb) ->
    Context.

get_sub_fields(Group) ->
    Res = maps:to_list(Group),
    proplists:substitute_aliases([], Res).

get_sub_fields_search(TypeInfo) ->
    maps:without([target_types,created_by_id,created_time,updated_by_id,updated_time],TypeInfo).

validate_type(ReqJson, Context) ->
    Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"type">>, Type).

validate_name(ReqJson, Context) ->
    Val = wh_json:get_value(<<"name">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"name">>, Val).