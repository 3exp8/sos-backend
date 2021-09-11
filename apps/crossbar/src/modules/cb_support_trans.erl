-module(cb_support_trans).
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


init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.support_trans">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.support_trans">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.support_trans">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.support_trans">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.support_trans">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.support_trans">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.support_trans">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.support_trans">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.support_trans">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.support_trans">>, ?MODULE, handle_delete).


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

authenticate_verb(Context, Path, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) -> true;

authorize_verb(Context, ?HTTP_PUT) ->
    true.

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Path, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context, Path);

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
    SupportTypes = support_trans_db:find_by_conditions([], PropQueryJson, Limit, Offset),
    PropSupportTypes = 
        lists:map(fun (Info) ->
            get_sub_fields(Info)
        end,SupportTypes),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, PropSupportTypes},
                         {fun cb_context:set_resp_status/2, success}])}.

handle_get({Req, Context}, Id) ->
    case support_trans_db:find(Id) of
      #{} = Info ->
          PropSupportTrans = get_sub_fields(Info),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, PropSupportTrans},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Trans not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end.

-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
    ReqJson = cb_context:req_json(Context),
    Uuid = zt_util:get_uuid(),    
    UserId = cb_context:customer_id(Context),
    lager:debug("ReqJson: ~p~n",[ReqJson]),

    SosRequestId = wh_json:get_value(<<"sos_request_id">>, ReqJson, <<>>),
    lager:debug("SosRequestId: ~p~n",[SosRequestId]),
    case sos_request_db:find(SosRequestId) of 
        notfound -> 
                cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Trans Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            requester_info := RequeterInfo
        } = SosRequestInfo -> 

            Type = wh_json:get_value(<<"type">>, ReqJson, <<"user">>),
            Id = wh_json:get_value(<<"id">>, ReqJson, <<>>),
            
            Info = #{
                id => <<"trans", Uuid/binary>>,
                sos_request_id => SosRequestId,
                requester_info => RequeterInfo,
                description =>  wh_json:get_value(<<"description">>, ReqJson, <<>>),
                medias => zt_util:to_map_list(wh_json:get_value(<<"medias">>, ReqJson,[])),
                support_list =>  zt_util:to_map_list(wh_json:get_value(<<"support_list">>, ReqJson,[])),
                support_time =>  wh_json:get_value(<<"support_time">>, ReqJson, zt_datetime:get_now()),
                supporter_info =>  sos_request_handler:get_supporter_info(Type,Id),
                created_time => zt_datetime:get_now(),
                created_by => UserId
            },
            support_trans_db:save(Info),
            maybe_update_support_status(Type, Id, SosRequestInfo, ReqJson),
            cb_context:setters(Context,
                            [{fun cb_context:set_resp_data/2, Info},
                                {fun cb_context:set_resp_status/2, success}])
    end.

maybe_update_support_status(Type, Id, SosRequestInfo, ReqJson) -> 
    case wh_json:get_value(<<"support_status">>, ReqJson, <<>>) of 
        <<>> -> 
            lager:debug("Do not update support status");
        SupportStatus -> 
        NewSosRequestInfo = sos_request_handler:maybe_update_support_status(Type, Id, SosRequestInfo,SupportStatus),
            sos_request_db:save(NewSosRequestInfo)
    end.


-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, Id) ->
    case support_trans_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Trans Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            support_time := SupportDateDb,
            description := DescriptionDb,
            support_list := SupportListDb,
            medias := MediasDb
        } = InfoDb -> 
         ReqJson =  cb_context:req_json(Context),
        NewMedias = 
            case wh_json:get_value(<<"medias">>, ReqJson, []) of
                [] ->  MediasDb;
                 MediasProps -> zt_util:to_map_list(MediasProps)
            end,
        NewSupportList =
        case wh_json:get_value(<<"support_list">>, ReqJson,[]) of 
         [] -> SupportListDb;
         SupportListProps -> 
                zt_util:to_map_list(SupportListProps)
        end,
            
         NewInfo = 
            maps:merge(InfoDb, #{
                medias => NewMedias,
                support_list => NewSupportList,
                description => wh_json:get_value(<<"description">>, ReqJson, DescriptionDb),
                support_time =>  wh_json:get_value(<<"support_time">>, ReqJson, SupportDateDb),
                updated_time => zt_datetime:get_now(),
                updated_by_id => cb_context:customer_id(Context)
            }),
            lager:debug("NewInfo: ~p~n",[NewInfo]),
            support_trans_db:save(NewInfo),
            cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                             ])            
    end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    case support_trans_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Trans Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        _ -> 
            support_trans_db:del_by_id(Id),
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
                    fun validate_sos_request_id/2,
                    fun validate_type/2,
                    fun validate_id/2
                ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Context, _Verb) ->
    Context.

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

validate_sos_request_id(ReqJson, Context) ->
    Type = wh_json:get_value(<<"sos_request_id">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"sos_request_id">>, Type).

validate_type(ReqJson, Context) ->
    Val = wh_json:get_value(<<"type">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"type">>, Val).

validate_id(ReqJson, Context) ->
    Val = wh_json:get_value(<<"id">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"id">>, Val).