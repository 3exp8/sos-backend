-module(cb_sos_request).

-include("crossbar.hrl").

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
         handle_delete/2,
         get_supporter_info/2,
         maybe_update_support_status/4
        ]).

-export([
         permissions/0
        ]).

-define(PATH_SEARCH, <<"search">>).
-define(PATH_SUPPORT, <<"support">>).

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
    [?HTTP_POST,?HTTP_PUT].

-spec resource_exists() -> true.
resource_exists() ->
    true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Id) ->
    true.

-spec resource_exists(path_token(),path_token()) -> true.
resource_exists(_Id, ?PATH_SUPPORT) ->
    true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    Token = cb_context:auth_token(Context),
    app_util:maybe_oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context, ?PATH_SEARCH) -> true;

authenticate(Context, Path) ->
    authenticate_verb(Context, Path, cb_context:req_verb(Context)).

authenticate(Context, Id, ?PATH_SUPPORT = Path) ->
    authenticate_verb(Context, Id, Path, cb_context:req_verb(Context)).

authenticate_verb(Context, Path, ?HTTP_GET) -> true;

authenticate_verb(Context, Path, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

authenticate_verb(Context, Id, ?PATH_SUPPORT, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context);
authorize_verb(Context, ?HTTP_PUT) ->
    true.
%authorize_util:authorize(?MODULE, Context).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Path, ?HTTP_GET) ->
    authorize_util:authorize(?MODULE, Context, Path);

authorize_verb(Context, ?PATH_SEARCH, ?HTTP_POST) -> true;

authorize_verb(Context, Path, ?HTTP_POST) ->
    authorize_util:authorize(?MODULE, Context, Path);

authorize_verb(Context, Path, ?HTTP_DELETE) ->
    authorize_util:authorize(?MODULE, Context, Path).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, Id, ?PATH_SUPPORT = Path) ->
    authorize_verb(Context, Id, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Id, ?PATH_SUPPORT = Path, _) -> true;



authorize_verb(Context, _Id, _Path, _) -> false.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).

validate(Context, Id, ?PATH_SUPPORT = Path) ->
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
            {Req,
             cb_context:setters(Context,
                                [{fun cb_context:set_resp_data/2, Db},
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
    RequesterType = wh_json:get_value(<<"requester_type">>, ReqJson, <<>>),
    RequesterId = wh_json:get_value(<<"requester_id">>, ReqJson, <<>>),
    case get_requester_info(RequesterId, RequesterType) of 
    {error, ErrorMsg} -> 
            cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, ErrorMsg},
            {fun cb_context:set_resp_status/2, 'error'},
            {fun cb_context:set_resp_error_code/2, 400}]
        );
    RequesterInfo -> 
        Uuid = zt_util:get_uuid(),
        Id = <<"request", Uuid/binary>>,
        BaseInfo = get_info(ReqJson, Context),
        Info = maps:merge(BaseInfo, #{
            id => Id,
            status => <<"waiting">>,
            verify_status => <<"pending">>,
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
        SortConds = deformat_sorts(SortCondsList, zt_util:to_bin(CurrentLocation), Unit),
        % lager:info("SortConds: ~p ~n",[SortConds]),
        Conds = build_search_conditions(CurrentLocation, ReqJson),
        lager:info("Sort: ~p ~n Conds: ~p ~n",[ SortConds, Conds]),
        QueryJson = cb_context:query_string(Context),
        Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
        Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
        lager:debug("search final conditions: ~p~n",[Conds]),
        {Total, Requests} = sos_request_db:find_count_by_conditions(Conds, SortConds, Limit, Offset),
        lager:info("Total Request: ~p ~n",[Requests]),
        PropRequests = lists:map(fun (Info) ->
                                    % BranchLocation = maps:get(location, Deal, <<>>),
                                    get_sub_fields(Info)
                               end,Requests),
        PropsRequestsWithTotal = #{
                                total => Total,
                                sos_requests => PropRequests
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
    ReqJson = cb_context:req_json(Context),
    
    case sos_request_db:find(Id) of
        #{
            support_types := SupportTypesDb,
            address_info := AddressInfoDb,
            contact_info := ContactInfoDb,
            medias := MediasDb,
            request_object_status := ObjectStatusDb,
            description := DescriptionDb,
            location := LocationDb
        } = RequestInfo ->

            %Doc = get_doc(ReqJson, RequesterId, Db),
            Description = wh_json:get_value(<<"description">>, ReqJson, DescriptionDb),
            SupportTypes = wh_json:get_value(<<"support_types">>, ReqJson, SupportTypesDb),
            Location = wh_json:get_value(<<"location">>, ReqJson, LocationDb),
            AddressInfo = cb_province:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson, AddressInfoDb)),
            
            NewContactInfo = 
                case wh_json:get_value(<<"contact_info">>, ReqJson, <<>>) of
                    <<>> ->  ContactInfoDb;
                    ContactInfoProps -> zt_util:to_map(ContactInfoProps)
                end,

            NewMedias = 
                case wh_json:get_value(<<"medias">>, ReqJson, []) of
                    [] ->  MediasDb;
                    MediaProps -> zt_util:to_map_list(MediaProps)
                end,

            NewObjectStatus = 
                case wh_json:get_value(<<"requester_object_status">>, ReqJson, []) of
                    [] ->  ObjectStatusDb;
                    ObjectStatusProps -> zt_util:to_map_list(ObjectStatusProps)
                end,

            UpdatedTime = zt_datetime:get_now(),
            NewInfo =  maps:merge(RequestInfo,#{
                            description => Description,
                            support_types => SupportTypes,
                            location => Location,
                            address_info => AddressInfo,
                            contact_info => NewContactInfo,
                            medias => NewMedias,
                            request_object_status => NewObjectStatus,
                            updated_by => app_util:get_requester_id(Context),
                            updated_time => UpdatedTime
            }),
            sos_request_db:save(NewInfo),
            cb_context:setters(Context,
                               [{fun cb_context:set_resp_data/2, NewInfo},
                                {fun cb_context:set_resp_status/2, success}]);
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
    case get_supporter_info(Type, SupporterId)  of 
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
                    UpdatedTime = zt_datetime:get_now(),
                    NewStatus = 
                        case StatusDb of 
                            <<"completed">> -> StatusDb;
                            _ -> <<"supporting">>
                        end,
                        
                    SuppoterInfo = maps:merge(BaseSupporterInfo, #{
                        schedule_support_date => wh_json:get_value(<<"support_date">>, ReqJson,<<>>),
                        description => wh_json:get_value(<<"description">>, ReqJson,<<>>),
                        status => <<"pending">>
                    }),
                    NewSupporters = [SuppoterInfo|SupportersDb],
                    NewInfo =  maps:merge(RequestInfo,#{
                            status => NewStatus,
                            supporters => NewSupporters,
                            updated_by => app_util:get_requester_id(Context),
                            updated_time => UpdatedTime
                    }),
                    sos_request_db:save(NewInfo),
                    cb_context:setters(Context,
                                    [{fun cb_context:set_resp_data/2, NewInfo},
                                        {fun cb_context:set_resp_status/2, success}]);
                _ ->
                    cb_context:setters(Context,
                                    [{fun cb_context:set_resp_error_msg/2, <<"SosRequest not found">>},
                                        {fun cb_context:set_resp_status/2, <<"error">>},
                                        {fun cb_context:set_resp_error_code/2, 404}])
            end
        end.

    handle_post(Context, Id, ?PATH_SUPPORT) ->
        ReqJson = cb_context:req_json(Context),
        Type = wh_json:get_value(<<"type">>, ReqJson,<<>>),
        SupporterId = wh_json:get_value(<<"supporter_id">>, ReqJson,<<>>),
        SupportStatus = wh_json:get_value(<<"support_status">>, ReqJson,<<>>),
       
        case sos_request_db:find(Id) of
            #{} = SosRequestInfo ->
                case maybe_update_support_status(Type, SupporterId, SosRequestInfo, SupportStatus) of 
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

maybe_update_support_status(Type, Id, _SosRequestInfo, <<>>) -> 
    lager:debug("Do not update support status: Type: ~p, Id: ~p~n",[Type, Id]),
    {error,no_change};

maybe_update_support_status(Type, Id, SosRequestInfo, SupportStatus) -> 
        
    Supporters = maps:get(supporters, SosRequestInfo, []),
    NewSupporters = 
        lists:map(fun(SupportInfo) -> 
                    case SupportInfo of 
                        #{
                            <<"type">> := Type,
                            <<"id">> := Id 
                        } ->
                            maps:merge(SupportInfo, #{
                                <<"status">> => SupportStatus,
                                <<"updated_time">> => zt_datetime:get_now()
                            });
                        _ -> SupportInfo
                    end
                end,Supporters),
    NewSosRequestInfo = 
        maps:merge(SosRequestInfo,#{
            supporters => NewSupporters
         }),
    {ok, NewSosRequestInfo}.

get_supporter_info(<<"group">>, Id) -> 
   case  group_db:find(Id) of 
   notfound -> {error,notfound};
  #{
        name := Name,
        contact_info := ContactInfo
  } -> 
    #{
        id => Id,
        type => <<"group">>,
        name => Name,
        contact_info => ContactInfo
    }
end;

get_supporter_info(<<"user">>, Id) -> 
   case  customer_db:find(Id) of 
   notfound -> {error,notfound};
  #{
        first_name := FirstName,
        last_name := LastName,
        phone_number := PhoneNumber
  } -> 
    #{
        id => Id,
        type => <<"user">>,
        name => FirstName,
        contact_info => #{
            first_name => FirstName,
            last_name => LastName,
            phone_number => PhoneNumber
        }
    };
Other -> lager:debug("other: ~p~n",[Other]),
{error,other}
end;

get_supporter_info(_, _) -> {error, not_support}.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    case sos_request_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"SOS request Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        _ -> 
            sos_request_db:del_by_id(Id),
            Resp = #{
                id => Id
            },
            cb_context:setters(Context,
                       [{fun cb_context:set_resp_data/2, Resp},
                        {fun cb_context:set_resp_status/2, success}])
    end.

permissions() ->
    authorize_util:default_permission(?MODULE).

build_search_conditions(CurLocation, ReqJson) -> 
    PriorityType = wh_json:get_value(<<"priority_type">>, ReqJson, <<>>),
    SupportTypes = wh_json:get_value(<<"support_types">>, ReqJson, <<>>),
    ObjectStatus = wh_json:get_value(<<"object_status">>, ReqJson, <<>>),
    SupportSstatus = wh_json:get_value(<<"status">>, ReqJson, <<>>),
    Keyword = wh_json:get_value(<<"keyword">>, ReqJson, <<>>),
    Distance = zt_util:to_integer(wh_json:get_value(<<"distance">>, ReqJson, 10)),
    SearchRequests = [
        {priority_type,PriorityType},
        {support_types,SupportTypes},
        {object_status,ObjectStatus},
        {support_status,SupportSstatus},
        {keyword,Keyword}
    ],
    DistanceCond = {location,'distance',{CurLocation, Distance, <<"km">>}},
    lists:foldl(fun({Type,Cond},Acc) -> 
        case build_condition(Type, Cond) of 
            ignore -> Acc;
            NewCondtion -> [NewCondtion|Acc]
        end
    end,[DistanceCond],SearchRequests).

build_condition(_,<<>>) -> ignore;

build_condition(priority_type, Val) -> 
    Vals = zt_util:split_string(Val),
    {priority_type,'in',Vals};

build_condition(support_types, Val) -> 
    Vals = zt_util:split_string(Val),
    {<<"support_types#type">>,'in',Vals};

build_condition(object_status, Val) -> 
    Vals = zt_util:split_string(Val),
    {<<"requester_object_status#type">>,'in',Vals};

build_condition(support_status, Val) -> 
    Vals = zt_util:split_string(Val),
    {<<"status">>,'in',Vals};

build_condition(keyword, Val) -> 
{'or',[{<<"subject">>,Val},{<<"description">>,Val}]};

build_condition(_,_) -> ignore.

deformat_sorts(Sorts, CurLocation, Unit) ->
    lists:map(fun (X) ->
        [Y] = X,
        {K, V} = Y,
        case K of
            <<"sort_distance">> ->
                {K, {CurLocation, V, Unit}};
            _ ->
                {K, V}
        end
    end, Sorts).

get_requester_info(_Id, <<"guest">>) -> #{};
get_requester_info(Id, <<"user">>) ->
    case customer_db:find(Id) of
        notfound ->
            {error,customer_notfound};
        CustomerInfo -> maps:with([id,first_name, last_name, phone_number],CustomerInfo)
    end;

get_requester_info(Id, <<"group">>) ->
    %%TODO
    case group_db:find(Id) of 
        notfound -> {error,group_notfound};
        GroupInfo -> maps:with([id,type,name],GroupInfo)
    end.

get_info(ReqJson, Context) ->
    
    Subject = wh_json:get_value(<<"subject">>, ReqJson, <<>>),
    PriorityType = wh_json:get_value(<<"priority_type">>, ReqJson, <<>>),
    Description = wh_json:get_value(<<"description">>, ReqJson, <<>>),
    SupportTypes = wh_json:get_value(<<"support_types">>, ReqJson, []),
    Location = wh_json:get_value(<<"location">>, ReqJson, <<"0.0,0.0">>),
    AddressInfo = cb_province:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson,[])),
    ContactInfo = zt_util:to_map(wh_json:get_value(<<"contact_info">>, ReqJson, [])),
    Medias = zt_util:to_map_list(wh_json:get_value(<<"medias">>, ReqJson, [])),
    ObjectStatus = zt_util:to_map_list(wh_json:get_value(<<"requester_object_status">>, ReqJson, [])),
    CreatedTime = zt_datetime:get_now(),
    #{
      subject => Subject,
      priority_type => PriorityType,
      description => Description,
      support_types => SupportTypes,
      location => Location,
      address_info => AddressInfo,
      contact_info => ContactInfo,
      requester_object_status => ObjectStatus,
      medias => Medias,
      created_by => app_util:get_requester_id(Context),
      created_time => CreatedTime
     }.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(Context, ?HTTP_PUT = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    validate_sos_request:send_sos_request(ReqJson, Context1);

validate_request(Context, _Verb) ->
    Context.

validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(?PATH_SEARCH, Context, ?HTTP_POST = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
        fun validate_search_long/2,
        fun validate_search_lat/2
    ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Id, Context, ?HTTP_POST = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [],
    lists:foldl(fun (F, C) ->
                        F(Id,ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, _Verb) ->
    Context.

validate_request(Id, ?PATH_SUPPORT, Context, ?HTTP_PUT = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [],
    lists:foldl(fun (F, C) ->
                        F(Id,ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(Id, ?PATH_SUPPORT, Context, ?HTTP_POST = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [],
    lists:foldl(fun (F, C) ->
                        F(Id,ReqJson, C)
                end,
    Context1,ValidateFuns);

validate_request(_Id, _, Context, _Verb) ->
                Context.
validate_search_lat(ReqJson, Context) ->
    LatPosition = wh_json:get_value(<<"lat_position">>, ReqJson, <<>>),

    case LatPosition of
        <<>> ->
            api_util:validate_error(Context, <<"lat_position">>, <<"required">>, <<"Field 'lat_position' is required">>);
        _  ->
            case zt_util:to_float(LatPosition) of
                Val when is_number(Val) andalso Val > -90 andalso Val < 90 ->
                    Context;
                _ ->
                    api_util:validate_error(Context, <<"lat_position">>, <<"invalid">>, <<"lat_position must be a number and value from -90 to 90">>)
            end
    end.

validate_search_long(ReqJson, Context) ->
    LongPosition  = wh_json:get_value(<<"long_position">>, ReqJson, <<>>),

    case LongPosition of
        <<>> ->
            api_util:validate_error(Context, <<"long_position">>, <<"required">>, <<"Field 'long_position' is required">>);
        _  ->
            case zt_util:to_float(LongPosition) of
                Val when is_number(Val) andalso Val > -180 andalso Val < 180 ->
                    Context;
                _ ->
                    api_util:validate_error(Context, <<"long_position">>, <<"invalid">>, <<"long_position must be a number and value from -180 to 180">>)
            end
    end.
get_sub_fields(Info) ->
    Fields = [created_by, updated_by, updated_time],
    maps:without(Fields, Info).
