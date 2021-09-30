-module(cb_sos_request).

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
         handle_put/3,
         handle_delete/2
        ]).

-export([
         permissions/0
        ]).

-define(PATH_SEARCH, <<"search">>).
-define(PATH_SUPPORT, <<"support">>).
-define(PATH_SUGGEST, <<"suggest">>).
-define(PATH_BOOKMARK, <<"bookmark">>).
-define(PATH_STATUS, <<"status">>).
-define(PATH_VERIFY, <<"verify">>).


init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.sos_requests">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.sos_requests">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.sos_requests">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.sos_requests">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.sos_requests">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.sos_requests">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.sos_requests">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.sos_requests">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.sos_requests">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.sos_requests">>, ?MODULE, handle_delete).

allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_Id, ?PATH_SUPPORT) ->
    [?HTTP_POST,?HTTP_PUT];

allowed_methods(_Id, ?PATH_SUGGEST) ->
    [?HTTP_POST];

allowed_methods(_Id, ?PATH_BOOKMARK) ->
    [?HTTP_POST];

allowed_methods(_Id, ?PATH_VERIFY) ->
    [?HTTP_POST];

allowed_methods(_Id, ?PATH_STATUS) ->
    [?HTTP_POST].

-spec resource_exists() -> true.
resource_exists() ->
    true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Id) ->
    true.

-spec resource_exists(path_token(),path_token()) -> true.
resource_exists(_Id, ?PATH_SUPPORT) -> true;

resource_exists(_Id, ?PATH_BOOKMARK) -> true;

resource_exists(_Id, ?PATH_STATUS) -> true;

resource_exists(_Id, ?PATH_VERIFY) -> true;

resource_exists(_Id, ?PATH_SUGGEST) -> true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    Token = cb_context:auth_token(Context),
    case app_util:oauth2_authentic(Token, Context) of 
        false -> true;
        Res -> Res
    end.

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, ?PATH_SEARCH) -> 
    Token = cb_context:auth_token(Context),
    case app_util:oauth2_authentic(Token, Context) of 
        false -> true;
        Res -> Res
    end;

authenticate(Context, Path) ->
    authenticate_verb(Context, Path, cb_context:req_verb(Context)).

authenticate(Context, _Id, ?PATH_SUGGEST) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context);

authenticate(Context, _Id, ?PATH_BOOKMARK) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context);

authenticate(Context, _Id, ?PATH_STATUS) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context);

authenticate(Context, _Id, ?PATH_VERIFY) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context);

authenticate(Context, Id, ?PATH_SUPPORT = Path) ->
    authenticate_verb(Context, Id, Path, cb_context:req_verb(Context)).

authenticate_verb(_Context, _Path, ?HTTP_GET) -> true;

authenticate_verb(Context, _Path, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

authenticate_verb(Context, _Id, ?PATH_SUPPORT, _) ->
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

authorize_verb(Context, _Path, ?HTTP_GET) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_ANY);

authorize_verb(_Context, ?PATH_SEARCH, ?HTTP_POST) -> true;

authorize_verb(Context, _Path, ?HTTP_POST) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE);

authorize_verb(Context, _Path, ?HTTP_DELETE) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, Id, Path) ->
    authorize_verb(Context, Id, Path, cb_context:req_verb(Context)).

authorize_verb(_Context, _Id, ?PATH_SUPPORT, _) -> true;

authorize_verb(Context, _Id, ?PATH_SUGGEST, ?HTTP_POST) -> 
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_OPERATOR_GE);

authorize_verb(Context, _Id, ?PATH_BOOKMARK, ?HTTP_POST) -> 
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_USER_GE);

authorize_verb(Context, _Id, ?PATH_VERIFY, ?HTTP_POST) -> 
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_OPERATOR_GE);    

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
    SosRequestsDb = sos_request_db:find_by_conditions([], PropQueryJson, Limit, Offset),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, SosRequestsDb},
                         {fun cb_context:set_resp_status/2, success}])}.

-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, Id) ->
    case sos_request_db:find(Id) of
        Db when is_map(Db) ->
            Info = get_sub_fields(Db),
            {Req,
             cb_context:setters(Context,
                                [{fun cb_context:set_resp_data/2, Info},
                                 {fun cb_context:set_resp_status/2, success}])};
        _ ->
            {Req,
             cb_context:setters(Context,
                                [{fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
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
            status => ?SOS_REQUEST_STATUS_OPEN,
            is_joined => false,
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
        Unit = <<"km">>,
        SortCondsList = wh_json:get_value(<<"sorts">>, ReqJson, [[{<<"sort_distance">>,asc}],[{<<"sort_priority">>,asc}]]),
        SortConds = sos_request_handler:deformat_sorts(SortCondsList, zt_util:to_bin(CurrentLocation), Unit),
        Conds = sos_request_handler:build_search_conditions(CurrentLocation, ReqJson),
        lager:info("Sort: ~p ~n Conds: ~p ~n",[ SortConds, Conds]),
        QueryJson = cb_context:query_string(Context),
        Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
        Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
        FinalConds = [{status,'not in',[?SOS_REQUEST_STATUS_RESOLVED,?SOS_REQUEST_STATUS_REJECTED]}|Conds],
        lager:debug("search FinalConds: ~p~n",[FinalConds]),
        FinalSortConds = [{<<"sort_created_time">>,asc}|SortConds],
        lager:debug("search FinalSortConds: ~p~n",[FinalSortConds]),
        {Total, Requests} = sos_request_db:find_count_by_conditions(FinalConds, FinalSortConds, Limit, Offset),
        lager:info("Total Request  ~p found ~n",[Total]),
        UserId = cb_context:user_id(Context),
        Role = cb_context:role(Context),
    
        FilteredGroups = group_handler:find_groups_by_user(UserId),
        FilteredRequests = 
            lists:map(fun(Info) -> 
                NewInfo = sos_request_handler:maybe_filter_bookmark(?OBJECT_TYPE_USER, UserId, Info),
                NewInfo2 = sos_request_handler:maybe_filter_bookmark_by_group(FilteredGroups, NewInfo),
                get_sub_fields(NewInfo2,[bookmarks]),
                IsSharePhoneNumber = maps:get(share_phone_number,NewInfo2,<<>>),
                sos_request_handler:maybe_hide_phone_number(Role, IsSharePhoneNumber,NewInfo2)
            end,Requests),

        PropsRequestsWithTotal = 
                #{
                    total => Total,
                    sos_requests => FilteredRequests
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
        RequestInfo when is_map(RequestInfo)->
            Role = cb_context:role(Context),
            UserId = cb_context:user_id(Context),
            case sos_request_handler:is_owner_or_admin(Role,UserId,RequestInfo) of 
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
                               [{fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])
    end.

handle_put(Context, Id, ?PATH_SUPPORT) ->
    ReqJson = cb_context:req_json(Context),
    Type = wh_json:get_value(<<"type">>, ReqJson,<<>>),
    SupporterId = wh_json:get_value(<<"supporter_id">>, ReqJson,<<>>),
    case sos_request_handler:get_supporter_info(Type, SupporterId,cb_context:user_id(Context))  of 
    {error, Error} -> 
        cb_context:setters(Context,
        [{fun cb_context:set_resp_error_msg/2, Error},
        {fun cb_context:set_resp_status/2, <<"error">>},
        {fun cb_context:set_resp_error_code/2, 400}]);
    BaseSupporterInfo -> 
            case sos_request_db:find(Id) of
                #{
                    status := StatusDb,
                    supporters := SupportersDb
                } = RequestInfo ->
                    case sos_request_handler:is_joined_request(Type,SupporterId,RequestInfo) of 
                        false -> 
                            UpdatedTime = zt_datetime:get_now(),
                            NewStatus = sos_request_handler:change_request_status(StatusDb, ?SOS_REQUEST_STATUS_ACCEPTED),
                            SupporterInfo = maps:merge(BaseSupporterInfo, #{
                                schedule_support_date => wh_json:get_value(<<"support_date">>, ReqJson,<<>>),
                                description => wh_json:get_value(<<"description">>, ReqJson,<<>>),
                                is_support_all => wh_json:get_value(<<"is_support_all">>, ReqJson,<<"false">>),
                                status => ?SOS_TASK_STATUS_OPEN
                            }),
                            NewSupporters = [SupporterInfo|SupportersDb],
                            NewInfo = maps:merge(RequestInfo,#{
                                    is_joined => true,
                                    status => NewStatus,
                                    supporters => NewSupporters,
                                    updated_by => app_util:get_requester_id(Context),
                                    updated_time => UpdatedTime
                            }),
                            sos_request_db:save(NewInfo),
                            cb_context:setters(Context,
                                            [{fun cb_context:set_resp_data/2, NewInfo},
                                                {fun cb_context:set_resp_status/2, success}]);
                        true -> 
                            cb_context:setters(Context,
                            [{fun cb_context:set_resp_error_msg/2, <<"You are always join this request">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 400}])
                    end;
                        
                _ ->
                    cb_context:setters(Context,
                                    [{fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                                        {fun cb_context:set_resp_status/2, <<"error">>},
                                        {fun cb_context:set_resp_error_code/2, 404}])
            end
        end.
    
    handle_post(Context, Id, ?PATH_SUGGEST) ->
        ReqJson = cb_context:req_json(Context),

        case sos_request_db:find(Id) of
            #{
                suggests := SuggestsDb
            } = Info ->

                Targets = wh_json:get_value(<<"targets">>, ReqJson,[]),
                UserId = cb_context:user_id(Context),
                SuggesterInfo = sos_request_handler:get_suggester_info(UserId),
                Note = wh_json:get_value(<<"note">>, ReqJson,<<>>),
                FilterTargets = 
                    lists:filtermap(fun(TargetProps)-> 
                        TargetType = proplists:get_value(<<"target_type">>, TargetProps),
                        TargetId = proplists:get_value(<<"target_id">>, TargetProps),
                        case sos_request_handler:get_suggest_request(TargetType,TargetId,SuggestsDb) of 
                            notfound ->
                                case sos_request_handler:get_target_type(TargetType, TargetId) of 
                                    {error, Error} -> false;
                                    {_, _, TargetName} ->
                                        SuggestInfo = 
                                            maps:merge(SuggesterInfo, #{
                                                target_type => TargetType,
                                                target_id => TargetId,
                                                target_name => TargetName,
                                                note => proplists:get_value(<<"note">>, TargetProps,Note),
                                                suggest_time => zt_datetime:get_now(),
                                                status => ?SOS_REQUEST_SUGGEST_STATUS_OPEN
                                            }),
                                            {true, SuggestInfo};
                                    _ ->
                                        false
                                end;
                                
                            SuggestInfoDb -> {true,SuggestInfoDb}    
                        end
                    
                    end,Targets),
                    NewInfo = 
                            maps:merge(Info, #{
                                suggests => FilterTargets
                            }),
                    lager:debug("NewInfo: ~p~n",[NewInfo]),
                    sos_request_db:save(NewInfo),
                
                    cb_context:setters(Context,
                                    [{fun cb_context:set_resp_data/2, NewInfo},
                                        {fun cb_context:set_resp_status/2, success}])
                
        end;

    handle_post(Context, Id, {?PATH_SUGGEST}) ->
        ReqJson = cb_context:req_json(Context),
        TargetType = wh_json:get_value(<<"target_type">>, ReqJson),
        TargetId = wh_json:get_value(<<"target_id">>, ReqJson),
        case sos_request_handler:get_target_type(TargetType, TargetId) of 
            {error, Error} ->
                cb_context:setters(Context,[
                    {fun cb_context:set_resp_error_msg/2, Error},
                    {fun cb_context:set_resp_status/2, <<"error">>},
                    {fun cb_context:set_resp_error_code/2, 400}
                ]);
            {_, _, TargetName} ->
                case sos_request_db:find(Id) of
                    #{
                        suggests := SuggestsDb
                    } = Info ->
                        case sos_request_handler:is_suggest_request(TargetType,TargetId,SuggestsDb) of 
                            false -> 
                                UserId = cb_context:user_id(Context),
                                SuggesterInfo = sos_request_handler:get_suggester_info(UserId),
                                SuggestInfo = 
                                    maps:merge(SuggesterInfo, #{
                                        target_type => TargetType,
                                        target_id => TargetId,
                                        target_name => TargetName,
                                        note => wh_json:get_value(<<"note">>, ReqJson,<<>>),
                                        suggest_time => zt_datetime:get_now(),
                                        status => ?SOS_REQUEST_SUGGEST_STATUS_OPEN
                                    }),

                                NewInfo = 
                                    maps:merge(Info, #{
                                        suggests => [SuggestInfo|SuggestsDb]
                                    }),
                                lager:debug("NewInfo: ~p~n",[NewInfo]),
                                sos_request_db:save(NewInfo),
                                cb_context:setters(Context,
                                            [{fun cb_context:set_resp_data/2, NewInfo},
                                                {fun cb_context:set_resp_status/2, success}]);
                            true ->
                                cb_context:setters(Context,
                                [{fun cb_context:set_resp_error_msg/2, <<"You are always suggest this request">>},
                                    {fun cb_context:set_resp_status/2, <<"error">>},
                                    {fun cb_context:set_resp_error_code/2, 400}])
                                    
                        end
                end;
            _ ->
                cb_context:setters(Context,[
                    {fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                    {fun cb_context:set_resp_status/2, <<"error">>},
                    {fun cb_context:set_resp_error_code/2, 404}
                ])
        end;

    handle_post(Context, Id, ?PATH_BOOKMARK) ->
        ReqJson = cb_context:req_json(Context),
        TargetType = wh_json:get_value(<<"bookmarker_type">>, ReqJson),
        TargetId = wh_json:get_value(<<"bookmarker_id">>, ReqJson),
        case sos_request_db:find(Id) of
            #{} = Info ->
                UserId = cb_context:user_id(Context),
                case sos_request_handler:get_my_target_type(TargetType, TargetId, UserId) of 
                    {error, Error} ->
                        cb_context:setters(Context,[
                            {fun cb_context:set_resp_error_msg/2, Error},
                            {fun cb_context:set_resp_status/2, <<"error">>},
                            {fun cb_context:set_resp_error_code/2, 400}
                        ]);

                    {_, _, BookmarkerName} ->
                        BookmarksDb = maps:get(bookmarks,Info,[]),
                        NewBookmarks = 
                            case wh_json:get_value(<<"action">>, ReqJson,<<"bookmark">>) of 
                                <<"bookmark">> -> 
                                    BookmarkInfo = 
                                    #{
                                        bookmarker_type => TargetType,
                                        bookmarker_id => TargetId,
                                        bookmarker_name => BookmarkerName,
                                        bookmark_time => zt_datetime:get_now()
                                    },
                                    sos_request_handler:maybe_add_bookmarks(BookmarksDb, BookmarkInfo);
                                <<"unbookmark">> -> 
                                    sos_request_handler:maybe_remove_bookmarks(BookmarksDb, TargetType, TargetId)
                            end,
                        NewInfo = 
                            maps:merge(Info, #{
                                bookmarks => NewBookmarks
                            }),
                        lager:debug("NewInfo: ~p~n",[NewInfo]),
                        sos_request_db:save(NewInfo),
                        cb_context:setters(Context,
                                    [{fun cb_context:set_resp_data/2, NewInfo},
                                        {fun cb_context:set_resp_status/2, success}])
                end;
            _ ->
                cb_context:setters(Context,[
                    {fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                    {fun cb_context:set_resp_status/2, <<"error">>},
                    {fun cb_context:set_resp_error_code/2, 404}
                ])
        end;

    handle_post(Context, Id,?PATH_VERIFY) ->
        case sos_request_db:find(Id) of 
            notfound -> 
                cb_context:setters(Context,
                    [{fun cb_context:set_resp_error_msg/2, <<"Sos Request Not Found">>},
                    {fun cb_context:set_resp_status/2, 'error'},
                    {fun cb_context:set_resp_error_code/2, 404}]
                );
            InfoDb -> 
             case sos_request_handler:maybe_verify(InfoDb, Context) of 
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
             end
        end;

handle_post(Context, Id,?PATH_STATUS) ->
    case sos_request_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Sos Request Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        InfoDb -> 
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
         end
    end;

    handle_post(Context, Id, ?PATH_SUPPORT) ->
        ReqJson = cb_context:req_json(Context),
        Type = wh_json:get_value(<<"type">>, ReqJson,<<>>),
        SupporterId = wh_json:get_value(<<"supporter_id">>, ReqJson,<<>>),
        SupportStatus = wh_json:get_value(<<"support_status">>, ReqJson,<<>>),
       
        case sos_request_db:find(Id) of
            #{} = SosRequestInfo ->
                case sos_request_handler:maybe_update_support_status(Type, SupporterId, SosRequestInfo, SupportStatus) of 
                        {ok, NewInfo} -> 
                            sos_request_db:save(NewInfo),
                            cb_context:setters(Context,
                                [{fun cb_context:set_resp_data/2, NewInfo},
                                {fun cb_context:set_resp_status/2, success}]);
                        {error, Error} ->
                            cb_context:setters(Context,
                                [{fun cb_context:set_resp_error_msg/2, Error},
                                 {fun cb_context:set_resp_status/2, <<"error">>},
                                 {fun cb_context:set_resp_error_code/2, 400}])
                end;
            _ ->
                        cb_context:setters(Context,
                                        [{fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                                            {fun cb_context:set_resp_status/2, <<"error">>},
                                            {fun cb_context:set_resp_error_code/2, 404}])
        end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    
    case sos_request_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"SOS request Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        Info -> 
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
            end
    end.

permissions() ->
    authorize_util:default_permission(?MODULE).

get_info(ReqJson, Context) ->
    
    Subject = wh_json:get_value(<<"subject">>, ReqJson, <<>>),
    PriorityType = wh_json:get_value(<<"priority_type">>, ReqJson, <<>>),
    Description = wh_json:get_value(<<"description">>, ReqJson, <<>>),
    SupportTypes = zt_util:to_map_list(wh_json:get_value(<<"support_types">>, ReqJson, [])),
    Location = wh_json:get_value(<<"location">>, ReqJson, <<"0.0,0.0">>),
    AddressInfo = province_handler:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson,[])),
    ContactInfo = zt_util:to_map(wh_json:get_value(<<"contact_info">>, ReqJson, [])),
    SharePhoneNumbebr = wh_json:get_value(<<"share_phone_number">>, ReqJson, ?SHARE_PHONE_NUMBER_TYPE_PRIVATE),
    Medias = zt_util:to_map_list(wh_json:get_value(<<"medias">>, ReqJson, [])),
    ObjectStatus = zt_util:to_map_list(wh_json:get_value(<<"requester_object_status">>, ReqJson, [])),
    Type = wh_json:get_value(<<"type">>, ReqJson, ?SOS_REQUEST_TYPE_ASK),
    #{
      type => Type,
      subject => Subject,
      priority_type => PriorityType,
      description => Description,
      support_types => sos_request_handler:filter_support_types(SupportTypes),
      color_info => sos_request_handler:calculate_color_type(Type, SupportTypes),
      location => Location,
      address_info => AddressInfo,
      contact_info => ContactInfo,
      share_phone_number => SharePhoneNumbebr,
      requester_object_status => ObjectStatus,
      medias => Medias,
      created_by => app_util:get_requester_id(Context),
      created_time => zt_datetime:get_now()
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
                    fun sos_request_handler:validate_request_type/2
                    ,fun sos_request_handler:validate_requester_type/2
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

validate_request(_Id, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_share_phone_number_update/2
    ],
    lists:foldl(fun (F, C) ->
            F(ReqJson, C)
        end,
    Context1,ValidateFuns);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, _Verb) ->
    Context.

validate_request(Id, ?PATH_SUPPORT, Context, ?HTTP_PUT) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [],
    lists:foldl(fun (F, C) ->
                        F(Id,ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(Id, ?PATH_SUPPORT, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [],
    lists:foldl(fun (F, C) ->
                        F(Id,ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, ?PATH_SUGGEST, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        
        fun sos_request_handler:validate_suggest_targets/2
        % fun sos_request_handler:validate_suggest_target_type/2,
        % fun sos_request_handler:validate_suggest_target_id/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, ?PATH_BOOKMARK, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        
        fun sos_request_handler:validate_bookmarker_type/2,
        fun sos_request_handler:validate_bookmarker_id/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, ?PATH_STATUS, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_update_status/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
    Context1,ValidateFuns);


validate_request(_Id, ?PATH_VERIFY, Context, ?HTTP_POST) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun sos_request_handler:validate_update_verify_status/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, _, Context, _Verb) ->
                Context.

get_sub_fields(Info) -> 
    get_sub_fields(Info,[]).


get_sub_fields(Info,OtherField) when is_atom(OtherField) ->
    get_sub_fields(Info,[OtherField]);

get_sub_fields(Info,OtherFields) when is_list(OtherFields) ->
    Fields = OtherFields ++ [created_by, updated_by, updated_time,suggest_info],
    maps:without(Fields, Info);

get_sub_fields(Info,OtherFieldType) -> 
   Field = zt_util:to_atom(OtherFieldType),
   get_sub_fields(Info,Field).