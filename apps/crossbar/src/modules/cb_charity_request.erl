-module(cb_charity_request).

-include("crossbar.hrl").
-include("app.hrl").

-export([init/0,
         allowed_methods/0,
         allowed_methods/1,
         allowed_methods/2,
         resource_exists/0,
         resource_exists/1,
         resource_exists/2,
         authenticate/1,
         authenticate/2,
         authenticate/3,
         authorize/1,
         authorize/2,
         authorize/3,
         validate/1,
         validate/2,
         validate/3
        ]).

-export([
         handle_get/1,
         handle_get/2,
         handle_put/1,
         handle_post/2,
         handle_post/3,
         handle_delete/2
        ]).

-export([
         permissions/0
        ]).


-define(PATH_SEARCH, <<"search">>).
-define(PATH_STATUS, <<"status">>).


init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.charity_requests">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.charity_requests">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.charity_requests">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.charity_requests">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.charity_requests">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.charity_requests">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.charity_requests">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.charity_requests">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.charity_requests">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.charity_requests">>, ?MODULE, handle_delete).

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].


allowed_methods(_Id, ?PATH_STATUS) ->
    [?HTTP_POST].

-spec resource_exists() -> true.
resource_exists() ->
    true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Id) ->
    true.

-spec resource_exists(path_token(),path_token()) -> true.

resource_exists(_Id, ?PATH_STATUS) -> true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    Token = cb_context:auth_token(Context),
    case app_util:oauth2_authentic(Token, Context) of 
        false -> true;
        Res -> Res
    end.

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context, ?PATH_SEARCH) -> true;

authenticate(Context, Path) ->
    authenticate_verb(Context, Path, cb_context:req_verb(Context)).

authenticate(Context, _Id, ?PATH_STATUS) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context). 

authenticate_verb(_Context, _Path, ?HTTP_GET) -> true;

authenticate_verb(Context, _Path, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context);

authorize_verb(_Context, ?HTTP_PUT) ->
    true.
%authorize_util:authorize(?MODULE, Context).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Path, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context);

authorize_verb(_Context, ?PATH_SEARCH, ?HTTP_POST) -> true;

authorize_verb(Context, Path, ?HTTP_POST) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE);

authorize_verb(Context, Path, ?HTTP_DELETE) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, Id, Path) ->
    authorize_verb(Context, Id, Path, cb_context:req_verb(Context)).

authorize_verb(Context, _Id, ?PATH_STATUS, ?HTTP_POST) -> 
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_USER_GE); 

authorize_verb(_Context, _Id, _Path, _) -> false.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Id, Path) ->
    validate_request(Id, Path, Context, cb_context:req_verb(Context)).

%%%%%%%%%%%%%%%%
%%  HANDLERS  %%
%%%%%%%%%%%%%%%%

-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>,QueryJson,?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
    CharityRequests = sos_request_db:find_by_conditions([{<<"type">>, ?SOS_REQUEST_TYPE_OFFER}], PropQueryJson, Limit, Offset),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, CharityRequests},
                         {fun cb_context:set_resp_status/2, success}])}.

-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, Id) ->
    case sos_request_db:find(Id) of
        #{type := ?SOS_REQUEST_TYPE_OFFER} = Db  ->
            {Req,
             cb_context:setters(Context,
                                [{fun cb_context:set_resp_data/2, Db},
                                 {fun cb_context:set_resp_status/2, success}])};
        _ ->
            {Req,
             cb_context:setters(Context,
                                [{fun cb_context:set_resp_error_msg/2, <<"Charity Request  not found">>},
                                 {fun cb_context:set_resp_status/2, <<"error">>},
                                 {fun cb_context:set_resp_error_code/2, 404}])}
    end.


-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->

    ReqJson = cb_context:req_json(Context),
    lager:debug("userid: ~p~n",[cb_context:user_id(Context)]),
    case sos_request_handler:get_requester_info(Context) of 
    {error, ErrorMsg} -> 
            cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, ErrorMsg},
            {fun cb_context:set_resp_status/2, 'error'},
            {fun cb_context:set_resp_error_code/2, 400}]
        );
    {RequesterType, RequesterInfo} -> 
        Uuid = zt_util:get_uuid(),
        Id = <<"request", Uuid/binary>>,
        BaseInfo = get_info(ReqJson, Context),
        Info = maps:merge(BaseInfo, #{
            id => Id,
            type => ?SOS_REQUEST_TYPE_OFFER,
            status => ?SOS_REQUEST_STATUS_OPEN,
            requester_type => RequesterType,
            requester_info => RequesterInfo
        }),
        sos_request_db:save(Info),
        cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, Info},
                            {fun cb_context:set_resp_status/2, success}])
    end.

-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().

handle_post(Context, ?PATH_SEARCH) ->
    try
        lager:info("sos request search: ~n",[]),
        ReqJson =  cb_context:req_json(Context),
        Long = zt_util:to_str(wh_json:get_value(<<"long_position">>, ReqJson, <<"0.0">>)),
        Lat = zt_util:to_str(wh_json:get_value(<<"lat_position">>, ReqJson, <<"0.0">>)),
        CurrentLocation  =  Lat ++ "," ++ Long,
        %Unit = proplists:get_value(<<"unit">>, ReqJson, <<>>),
        Unit = <<"km">>,
        % Distance = zt_util:to_str(proplists:get_value(<<"distance">>, Data, <<"5">>)S),
        SortCondsList = wh_json:get_value(<<"sorts">>, ReqJson, []),
        SortConds = sos_request_handler:deformat_sorts(SortCondsList, zt_util:to_bin(CurrentLocation), Unit),
        % lager:info("SortConds: ~p ~n",[SortConds]),
        Conds = sos_request_handler:build_search_conditions(CurrentLocation, ReqJson),
        lager:info("Sort: ~p ~n Conds: ~p ~n",[ SortConds, Conds]),
        QueryJson = cb_context:query_string(Context),
        Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
        Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
        lager:debug("search final conditions: ~p~n",[Conds]),
        {Total, Requests} = sos_request_db:find_count_by_conditions([{<<"type">>, ?SOS_REQUEST_TYPE_OFFER} | Conds], SortConds, Limit, Offset),
        lager:info("Total Request: ~p ~n",[Requests]),
        UserId = cb_context:user_id(Context),
    
        FilteredGroups = group_handler:find_groups_by_user(UserId),

        FilteredRequests = 
                lists:map(fun(Info) -> 
                    NewInfo = sos_request_handler:maybe_filter_bookmark(?OBJECT_TYPE_USER, UserId, Info),
                    NewInfo2 = sos_request_handler:maybe_filter_bookmark_by_group(FilteredGroups, NewInfo),
                    get_sub_fields(NewInfo2,[bookmarks])
                end,Requests),

        PropsRequestsWithTotal = 
                #{
                    total => Total,
                    charity_requests => FilteredRequests
                },
        cb_context:setters(Context
                           ,[{fun cb_context:set_resp_data/2, PropsRequestsWithTotal}
                             ,{fun cb_context:set_resp_status/2, 'success'}
                            ])
    catch
        _:_ -> cb_context:setters(Context
                                  ,[{fun cb_context:set_resp_status/2, 'error'}
                                    ,{fun cb_context:set_resp_error_code/2, 400}
                                    ,{fun cb_context:set_resp_error_msg/2, <<"Bad request">>}
                                   ])
    end;


handle_post(Context, Id) ->
    case sos_request_db:find(Id) of
        #{
            type := ?SOS_REQUEST_TYPE_OFFER
        } = RequestInfo ->
            Role = cb_context:role(Context),
            UserId = cb_context:user_id(Context),
            case sos_request_handler:is_owner_or_admin(Role, UserId, RequestInfo) of 
                true -> 
                    NewInfo = sos_request_handler:maybe_update_request_info(Context, RequestInfo),
                    cb_context:setters(Context,
                                    [{fun cb_context:set_resp_data/2, NewInfo},
                                        {fun cb_context:set_resp_status/2, success}]);
                false -> 
                    cb_context:setters(Context,
                    [{fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                    {fun cb_context:set_resp_status/2, <<"error">>},
                    {fun cb_context:set_resp_error_code/2, 403}])
            end;
        _ ->
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_error_msg/2, <<"Charity Request not found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])
    end.



handle_post(Context, Id,?PATH_STATUS) ->
    case sos_request_db:find(Id) of 
        #{
            type := ?SOS_REQUEST_TYPE_OFFER
        } = InfoDb -> 
         case sos_request_handler:maybe_update_request_status(InfoDb, Context) of 
            {success,NewInfo} -> 
                cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, NewInfo}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]);  
                {warning,WarningMsg} ->
                    Context2 = api_util:validate_error(Context, <<"status">>, <<"invalid">>, WarningMsg),
                    cb_context:setters(Context2,[
                        {fun cb_context:set_resp_error_msg/2, WarningMsg},
                        {fun cb_context:set_resp_status/2, <<"error">>},
                        {fun cb_context:set_resp_error_code/2, 400}
                    ]);
                {error,forbidden} -> 
                    Context2 = api_util:validate_error(Context, <<"status">>, <<"forbidden">>, forbidden),
                    cb_context:setters(Context2,[
                        {fun cb_context:set_resp_error_msg/2, forbidden},
                        {fun cb_context:set_resp_status/2, <<"error">>},
                        {fun cb_context:set_resp_error_code/2, 403}
                    ]);
                {error,Error} -> 
                    Context2 = api_util:validate_error(Context, <<"status">>, <<"invalid">>, Error),
                    cb_context:setters(Context2,[
                        {fun cb_context:set_resp_error_msg/2, Error},
                        {fun cb_context:set_resp_status/2, <<"error">>},
                        {fun cb_context:set_resp_error_code/2, 400}
                    ])
         end;
    _ ->
        cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, <<"Charity Request Not Found">>},
            {fun cb_context:set_resp_status/2, 'error'},
            {fun cb_context:set_resp_error_code/2, 404}]
        )
    end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    case sos_request_db:find(Id) of 
         #{
             type := ?SOS_REQUEST_TYPE_OFFER
            } = Info -> 
            Role = cb_context:role(Context),
            UserId = cb_context:user_id(Context),
            case sos_request_handler:is_owner_or_admin(Role, UserId, Info) of 
                true ->
                    sos_request_db:del_by_id(Id),
                    Resp = #{
                        id => Id
                    },
                    cb_context:setters(Context,
                            [{fun cb_context:set_resp_data/2, Resp},
                                {fun cb_context:set_resp_status/2, success}]);
                false ->
                    cb_context:setters(Context,
                    [{fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                        {fun cb_context:set_resp_status/2, <<"error">>},
                        {fun cb_context:set_resp_error_code/2, 403}])
            end;
        _ -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Charity request Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            )
    end.

permissions() ->
    authorize_util:default_permission(?MODULE).


get_info(ReqJson, Context) ->
    
    Subject = wh_json:get_value(<<"subject">>, ReqJson, <<>>),
    Description = wh_json:get_value(<<"description">>, ReqJson, <<>>),
    SupportTypes = wh_json:get_value(<<"support_types">>, ReqJson, []),
    Location = wh_json:get_value(<<"location">>, ReqJson, <<"0.0,0.0">>),
    AddressInfo = cb_province:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson,[])),
    ContactInfo = zt_util:to_map(wh_json:get_value(<<"contact_info">>, ReqJson, [])),
    SharePhoneNumbebr = wh_json:get_value(<<"share_phone_number">>, ReqJson, ?SHARE_PHONE_NUMBER_TYPE_PRIVATE),
    Medias = zt_util:to_map_list(wh_json:get_value(<<"medias">>, ReqJson, [])),
    CreatedTime = zt_datetime:get_now(),
    #{
      subject => Subject,
      description => Description,
      support_types => SupportTypes,
      location => Location,
      address_info => AddressInfo,
      contact_info => ContactInfo,
      share_phone_number => SharePhoneNumbebr,
      medias => Medias,
      created_by => app_util:get_requester_id(Context),
      created_time => CreatedTime
     }.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%


validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(Context, ?HTTP_PUT) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context
                                  ,[{fun cb_context:set_resp_status/2, 'success'}]),    
    ValidateFuns = [
                     fun sos_request_handler:validate_requester_type/2
                    ,fun sos_request_handler:validate_share_phone_number/2
                   ],
    lists:foldl(fun(F, C) ->
                    F(ReqJson, C)
                end, Context1,  ValidateFuns);

validate_request(Context, _Verb) ->
    Context.

validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(?PATH_SEARCH, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_search_long/2,
        fun sos_request_handler:validate_search_lat/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Id, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_share_phone_number_update/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, _Verb) ->
    Context.


validate_request(Id, ?PATH_STATUS, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_update_status/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, _, Context, _Verb) ->
                Context.

get_sub_fields(Info,OtherField) when is_atom(OtherField) ->
    get_sub_fields(Info,[OtherField]);

get_sub_fields(Info,OtherFields) when is_list(OtherFields) ->
    Fields = OtherFields ++ [created_by, updated_by, updated_time],
    maps:without(Fields, Info);

get_sub_fields(Info,OtherFieldType) -> 
   Field = zt_util:to_atom(OtherFieldType),
   get_sub_fields(Info,Field).

get_sub_fields(Info) ->
    get_sub_fields(Info,[]).
