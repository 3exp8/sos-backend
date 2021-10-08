-module(cb_group).
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
    handle_get/3,
    handle_put/1, 
    handle_post/2,
    handle_post/3, 
    handle_delete/2,
    handle_delete/3,
    errors/0
]).

-export([
          permissions/0
  ]).

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.groups">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.groups">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.groups">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.groups">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.groups">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.groups">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.groups">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.groups">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.groups">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.groups">>, ?MODULE, handle_delete).

-define(PATH_VERIFY, <<"verify">>).
-define(PATH_MEMBER, <<"members">>).
-define(PATH_TYPE, <<"types">>).
-define(PATH_SUGGEST, <<"suggest">>).
-define(PATH_BOOKMARK, <<"bookmark">>).
-define(PATH_SEARCH, <<"search">>).

allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Path) -> [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_Id,?PATH_VERIFY) -> [?HTTP_POST];

allowed_methods(_Id,?PATH_MEMBER) ->
    [?HTTP_POST, ?HTTP_DELETE];

allowed_methods(_Id,?PATH_SUGGEST) -> [?HTTP_GET];
allowed_methods(_Id,?PATH_BOOKMARK) -> [?HTTP_GET].

-spec resource_exists() -> true.
resource_exists() -> true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Path) -> true.

-spec resource_exists(path_token(),path_token()) -> true.
resource_exists(_Id,?PATH_BOOKMARK) -> true;
resource_exists(_Id,?PATH_SUGGEST) -> true;
resource_exists(_Id,?PATH_VERIFY) -> true;
resource_exists(_Id,?PATH_MEMBER) -> true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context, ?PATH_SEARCH) ->true;

authenticate(Context, _Path) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(Context, _Id, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_OPERATOR_GE);

authorize_verb(_Context, ?HTTP_PUT) -> true.

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(_Context, _Path, ?HTTP_GET) -> true;

authorize_verb(_Context, ?PATH_SEARCH, ?HTTP_POST) -> true;

authorize_verb(Context, _Path, ?HTTP_POST) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE);

authorize_verb(Context, _Path, ?HTTP_DELETE) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Id, ?PATH_VERIFY) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_OPERATOR_GE);

authorize(Context, _Id, ?PATH_BOOKMARK) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role,?USER_ROLE_USER_GE);

authorize(Context, _Id, ?PATH_SUGGEST = Path) ->
    Role = cb_context:role(Context),
    authorize_verb(Context, Role, Path, cb_context:req_verb(Context));

authorize(_Context, _Id, ?PATH_MEMBER) -> true.

authorize_verb(_Context, Role, ?PATH_SUGGEST, ?HTTP_GET) -> 
    authorize_util:check_role(Role,?USER_ROLE_USER_GE);

authorize_verb(_Context, Role, ?PATH_SUGGEST, _) -> 
    authorize_util:check_role(Role,?USER_ROLE_OPERATOR_GE).

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
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
    Groups = group_db:find_by_conditions([], PropQueryJson, Limit, Offset),
    PropGroups = 
        lists:map(fun (Info) ->
            get_sub_fields(Info)
        end,Groups),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, PropGroups},
                         {fun cb_context:set_resp_status/2, success}])}.

-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, ?PATH_TYPE) ->
    Types = [],
    
    {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, Types},
                               {fun cb_context:set_resp_status/2, success}])};

handle_get({Req, Context}, Id) ->
    case group_db:find(Id) of
      #{} = Info ->
          PropGroup = get_sub_fields(Info),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, PropGroup},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Group not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end.

% sos_request_db:find_by_conditions([{<<"bookmarks#bookmarker_type">>,<<"group">>},{<<"bookmarks#bookmarker_id">>,<<"group372dfa628f08303797acb05751ddfc9a">>}], [], 5, 0).
handle_get({Req, Context}, Id, ?PATH_BOOKMARK) ->
    case group_db:find(Id) of
      #{} ->
            QueryJson = cb_context:query_string(Context),
            Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
            Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
            PropQueryJson = wh_json:to_proplist(QueryJson),
            SosRequests = 
                sos_request_db:find_by_conditions([
                    {<<"bookmarks.bookmarker_type">>,?OBJECT_TYPE_GROUP},
                    {<<"bookmarks.bookmarker_id">>,Id}
                ], PropQueryJson, Limit, Offset),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, SosRequests},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Group not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end;

handle_get({Req, Context}, Id, ?PATH_SUGGEST) ->
    case group_db:find(Id) of
      #{} ->
            QueryJson = cb_context:query_string(Context),
            Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
            Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
            PropQueryJson = wh_json:to_proplist(QueryJson),
            SosRequests = 
                sos_request_db:find_by_conditions([
                    {'or',
                        [
                            % {
                            %     'and',[
                            %         {<<"suggests#target_type">>,?OBJECT_TYPE_GROUP},
                            %         {<<"suggests#target_id">>,Id}
                            %     ]
                            % },
                            {
                                'and',[
                                    {<<"suggests.target_type">>,?OBJECT_TYPE_GROUP},
                                    {<<"suggests.target_id">>,Id}
                                ]
                            }

                        ]
                    }
                ], PropQueryJson, Limit, Offset),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, SosRequests},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"Group not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end.

-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
    ReqJson = cb_context:req_json(Context),
    Uuid = zt_util:get_uuid(),
    UserId = cb_context:user_id(Context),
    UserInfo = user_db:find(UserId),
    UserInfoFiltered = maps:with([phone_number, id, first_name, last_name],UserInfo),
    ContactInfo = zt_util:to_map(wh_json:get_value(<<"contact_info">>, ReqJson,[])),
    PhoneNumber = maps:get(phone_number, ContactInfo, <<>>),
    Role = cb_context:role(Context),
    case otp_handler:otp_valid(Role, <<>>,PhoneNumber,wh_json:get_value(<<"confirm_code">>, ReqJson,<<>>)) of 
    true -> 
        InfoBase = get_info(ReqJson,UserId),
        AdminInfo = maps:merge(UserInfoFiltered,#{
            role => ?GROUP_USER_ROLE_ADMIN
        }),
        Info = maps:merge(InfoBase, #{
            id => <<"group", Uuid/binary>>,
            contact_info => ContactInfo,
            members => [AdminInfo]
        }),
    
        group_db:save(Info),
        cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, Info},
                            {fun cb_context:set_resp_status/2, success}]);
    _ -> 
        cb_context:setters(Context,[
                {fun cb_context:set_resp_error_msg/2, <<"otp_invalid">>},
                {fun cb_context:set_resp_status/2, <<"error">>},
                {fun cb_context:set_resp_error_code/2, 400}
            ])
    end.

-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, ?PATH_SEARCH) ->
    try
        lager:info("group search: ~n",[]),
        ReqJson =  cb_context:req_json(Context),
        Long = zt_util:to_str(wh_json:get_value(<<"long_position">>, ReqJson, <<"0.0">>)),
        Lat = zt_util:to_str(wh_json:get_value(<<"lat_position">>, ReqJson, <<"0.0">>)),
        CurrentLocation  =  Lat ++ "," ++ Long,
        Unit = <<"km">>,
        SortCondsList = wh_json:get_value(<<"sorts">>, ReqJson, [[{<<"sort_distance">>,asc}]]),
        SortConds = group_handler:deformat_sorts(SortCondsList, zt_util:to_bin(CurrentLocation), Unit),
        Conds = group_handler:build_search_conditions(CurrentLocation, ReqJson),
        lager:info("Sort: ~p ~n Conds: ~p ~n",[ SortConds, Conds]),
        QueryJson = cb_context:query_string(Context),
        Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
        Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
        FinalConds = Conds,
        lager:debug("search FinalConds: ~p~n",[FinalConds]),
        %FinalSortConds = [{<<"sort_created_time">>,asc}|SortConds],
        FinalSortConds = SortConds,
        lager:debug("search FinalSortConds: ~p~n",[FinalSortConds]),
        {Total, Groups} = group_db:find_count_by_conditions(FinalConds, FinalSortConds, Limit, Offset),
        lager:info("Total Request  ~p found ~n",[Total]),

        FilteredGroups = 
            lists:map(fun (Info) ->
                get_sub_fields_search(Info)
            end,Groups),
        PropsGroupsWithTotal = 
                #{
                    total => Total,
                    groups => FilteredGroups
                },
        cb_context:setters(Context
                           ,[{fun cb_context:set_resp_data/2, PropsGroupsWithTotal}
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
    case group_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Group Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            name := NameDb,
            location := LocationDb,
            avatar := AvatarDb,
            address_info := AddressInfoDb,
            contact_info := ContactInfoDb,
            detail_info := DetailInfoDb,
            description := DescriptionDb
        } = InfoDb -> 
        UserId = cb_context:user_id(Context),
        case group_handler:is_group_admin(Id,UserId) of 
            true -> 
                ReqJson =  cb_context:req_json(Context),
                
                NewContactInfo = 
                    case wh_json:get_value(<<"contact_info">>, ReqJson, <<>>) of
                        <<>> ->  ContactInfoDb;
                        ContactInfoProps -> 
                            zt_util:to_map(ContactInfoProps)
                    end,
                PhoneNumberdb = maps:get(<<"phone_number">>, ContactInfoDb, <<>>),
                NewPhoneNumber = maps:get(<<"phone_number">>, NewContactInfo, <<>>),
                Role = cb_context:role(Context),
                case otp_handler:otp_valid(Role,PhoneNumberdb,NewPhoneNumber,wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>)) of 
                    true -> 
                        NewDetailInfo = 
                            case wh_json:get_value(<<"detail_info">>, ReqJson, <<>>) of
                                <<>> ->  DetailInfoDb;
                                DetailInfoProps -> zt_util:to_map(DetailInfoProps)
                            end,

                        NewInfo = 
                            maps:merge(InfoDb, #{
                                name => wh_json:get_value(<<"name">>, ReqJson,NameDb),
                                location => wh_json:get_value(<<"location">>, ReqJson, LocationDb),
                                avatar => wh_json:get_value(<<"avatar">>, ReqJson, AvatarDb),
                                address_info => province_handler:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson,AddressInfoDb)),
                                contact_info => NewContactInfo,
                                detail_info => NewDetailInfo,
                                description => wh_json:get_value(<<"description">>, ReqJson,DescriptionDb),         
                                updated_time => zt_datetime:get_now(),
                                updated_by => cb_context:user_id(Context)
                            }),
                        group_db:save(NewInfo),
                        cb_context:setters(Context
                                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                                            ,{fun cb_context:set_resp_status/2, 'success'}
                                            ]);
                    false -> 
                        cb_context:setters(Context,[{fun cb_context:set_resp_error_msg/2, <<"otp_invalid">>},
                            {fun cb_context:set_resp_status/2, <<"error">>},
                            {fun cb_context:set_resp_error_code/2, 400}]
                            )
                end;
            false ->
                cb_context:setters(Context,[{fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                                            {fun cb_context:set_resp_status/2, <<"error">>},
                                            {fun cb_context:set_resp_error_code/2, 403}]
                                    )
        end                        
    end.

-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, Id,?PATH_VERIFY) ->
    case group_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Group Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        InfoDb -> 
         ReqJson =  cb_context:req_json(Context),
         UserId =  cb_context:user_id(Context),
         #{
            email := Email,
            first_name := FirstName,
            last_name := LastName
         } = user_db:find(UserId),
         NewInfo = 
            maps:merge(InfoDb, #{
                verify_status => wh_json:get_value(<<"verify_status">>, ReqJson),
                verify_info => #{
                    user_id => UserId,
                    email => Email,
                    first_name => FirstName,
                    last_name => LastName,
                    verify_description => wh_json:get_value(<<"verify_description">>, ReqJson,<<>>),
                    time => zt_util:now_to_utc_binary(os:timestamp())
                }
            }),
         group_db:save(NewInfo),
         cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                             ])            
    end;

handle_post(Context, Id,?PATH_MEMBER) ->
    case group_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Group Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            members := MembersDb
        } = InfoDb ->
            UserId = cb_context:user_id(Context),
            case group_handler:is_group_admin(Id, UserId) of 
                true ->
                    ReqJson =  cb_context:req_json(Context),
                    Members = wh_json:get_value(<<"members">>, ReqJson, []),
                    FilteredMembers = 
                    lists:filtermap(fun(MemberProps) -> 
                            lager:debug("MemberProps: ~p~n",[MemberProps]),
                            MemberMap = zt_util:map_keys_to_atom(zt_util:to_map(MemberProps)),
                            GroupUserId = maps:get(id,MemberMap),
                            UserInfo = user_db:find(GroupUserId),
                            UserInfoFiltered = maps:with([phone_number, id, first_name, last_name],UserInfo),
                            MemberInfo = maps:merge(UserInfoFiltered,#{
                                role => maps:get(role,MemberMap,<<>>)
                            }),
                            {true, MemberInfo}
                    end,Members),
                    lager:debug("FilteredMembers: ~p~n",[FilteredMembers]),
                    NewMembers = zt_util:merge_list_maps(id,MembersDb ++ FilteredMembers),
                    NewInfo = maps:merge(InfoDb, #{
                        members => NewMembers
                    }),
                    group_db:save(NewInfo),
                    cb_context:setters(Context
                                        ,[{fun cb_context:set_resp_data/2, NewInfo}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
                false ->
                    cb_context:setters(Context,
                                            [{fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                                                {fun cb_context:set_resp_status/2, <<"error">>},
                                                {fun cb_context:set_resp_error_code/2, 403}]
                                        )
            end
    end.

handle_delete(Context, Id, ?PATH_MEMBER) ->
    lager:debug("delete group member: id: ~p~n",[Id]),
    case group_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Group Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            members := MembersDb
        } = InfoDb -> 

        UserId = cb_context:user_id(Context),
        case group_handler:is_group_admin(Id, UserId) of 
            true ->
                    ReqJson =  cb_context:req_json(Context),
                    Members = wh_json:get_value(<<"members">>, ReqJson, []),
                    MembersMapDb = zt_util:to_map_with_key(<<"id">>,MembersDb),
                    FilteredMembersMap = 
                        lists:foldl(fun(MemberProps, FilteredMembersMapDb) -> 
                                MemberMap = zt_util:to_map(MemberProps),
                                UserId = maps:get(id,MemberMap),
                                maps:remove(UserId, FilteredMembersMapDb)
                        end,MembersMapDb,Members),
                    NewMembers = maps:values(FilteredMembersMap),
                    NewInfo = maps:merge(InfoDb, #{
                        members => NewMembers
                    }),
                    group_db:save(NewInfo),
                    cb_context:setters(Context
                                        ,[{fun cb_context:set_resp_data/2, NewInfo}
                                        ,{fun cb_context:set_resp_status/2, 'success'}
                                        ]);
            false -> 
                cb_context:setters(Context,[
                        {fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                        {fun cb_context:set_resp_status/2, <<"error">>},
                        {fun cb_context:set_resp_error_code/2, 403}
                    ])
        end
    end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().

handle_delete(Context, Id) ->

    case group_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Group Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        _ -> 
            UserId = cb_context:user_id(Context),
            case group_handler:is_group_admin(Id, UserId) of 
                true -> 
                    group_db:del_by_id(Id),
                    Resp = #{
                        id => Id
                    },
                    cb_context:setters(Context,
                            [{fun cb_context:set_resp_data/2, Resp},
                                {fun cb_context:set_resp_status/2, success}]);
                false ->
                    cb_context:setters(Context,[
                            {fun cb_context:set_resp_error_msg/2, <<"forbidden">>},
                            {fun cb_context:set_resp_status/2, <<"error">>},
                            {fun cb_context:set_resp_error_code/2, 403}
                        ])
            end
    end.

permissions() ->
  authorize_util:default_permission(?MODULE).
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

get_info(ReqJson,UserId) -> 
    Type = zt_util:normalize_string(wh_json:get_value(<<"type">>, ReqJson)),
    AddressInfo = province_handler:get_address_detail_info(wh_json:get_value(<<"address_info">>, ReqJson,[])),
    CreateTime  =  zt_datetime:get_now(),
    #{
        type => Type,
        name => wh_json:get_value(<<"name">>, ReqJson,<<>>),
        location => wh_json:get_value(<<"location">>, ReqJson, <<"0.0,0.0">>),
        avatar => wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
        address_info => AddressInfo,
        detail_info => filter_detail_info(zt_util:to_map(wh_json:get_value(<<"detail_info">>, ReqJson,[]))),
        admin_id => <<>>,
        members => [],
        verify_status => <<"pending">>, 
        description => wh_json:get_value(<<"description">>, ReqJson,<<>>),         
        created_time => CreateTime, 
        updated_time => CreateTime,
        created_by => UserId,
        updated_by => UserId
    }.

filter_detail_info(DetailInfo) -> 
    SupportTypesList = maps:get(support_types, DetailInfo,[]),
    SupportTypesFiltered = 
        lists:map(fun(SupportTypeProps) -> 
            lager:debug("SupportTypeProps: ~p~n",[SupportTypeProps]),
            SupportTypeMap = maps:from_list(SupportTypeProps),
            maps:with([<<"type">>,<<"name">>],SupportTypeMap)
        end,SupportTypesList),
    maps:merge(DetailInfo, #{
        support_types => SupportTypesFiltered
    }).


validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(Context, ?HTTP_PUT) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun group_handler:validate_type/2,
                    fun group_handler:validate_name/2,
                    fun group_handler:validate_contact_info/2,
                    fun group_handler:validate_detail_info/2],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Context, _Verb) ->
    Context.

validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(?PATH_SEARCH, Context, ?HTTP_POST = _Verb) ->

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

validate_request(_Id, Context, ?HTTP_POST = _Verb) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, _Verb) ->
    Context.

validate_request(_Id, ?PATH_VERIFY, Context, ?HTTP_POST) ->

    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun group_handler:validate_verify_status/2],
                   
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(_Id, ?PATH_MEMBER, Context, ?HTTP_POST) ->

    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun group_handler:validate_add_members/2
                ],
                   
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(_Id, ?PATH_MEMBER, Context, ?HTTP_DELETE) ->

                ReqJson = cb_context:req_json(Context),
                Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
                ValidateFuns = [
                                fun group_handler:validate_remove_members/2],
                               
                lists:foldl(fun (F, C) ->
                                    F(ReqJson, C)
                            end,
                            Context1,
                            ValidateFuns);

validate_request(_Id, ?PATH_SUGGEST, Context, _) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, ?PATH_BOOKMARK, Context, _) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, _Path, Context, _Verb) ->
    Context.

get_sub_fields(Info) ->
    Fields =  [distance],
    maps:without(Fields, Info).

get_sub_fields_search(Info)->
    Fields =  [members, created_by, updated_by, updated_time,suggest_info],
    maps:without(Fields, Info).

errors() -> 
  Path = <<"groups">>,
  Path1 = <<Path/binary,"/{id}">>,
  HandleGet = app_util:declare_api_validate(<<"get">>,Path,[]),
  HandlePutValidates =
  [
    {<<"type">>, <<"required">>, <<"Field 'type' is required">>},
    {<<"name">>, <<"required">>, <<"Field 'name' is required">>},
    {<<"contact_info">>, <<"required">>, <<"Field 'contact_info' is required">>},
    {<<"detail_info">>, <<"required">>, <<"Field 'detail_info' is required">>}
   
  ],
  HandlePut = app_util:declare_api_validate(<<"put">>,Path,HandlePutValidates),
  HandleGet1 = app_util:declare_api_validate(<<"get">>,Path1,[]),
  HandlePost1Validates =
  [
   
  ],
  HandlePost1 = app_util:declare_api_validate(<<"post">>,Path1,HandlePost1Validates),
  
  PathVerify = ?PATH_VERIFY,
  HandlePost2VerifyValidates = [{<<"verify_status">>, <<"required">>, <<"Field 'verify_status' is required">>}],
  HandlePost2Verify = app_util:declare_api_validate(<<"post">>,<<Path1/binary,"/",PathVerify/binary>>,HandlePost2VerifyValidates),

  PathMember = ?PATH_MEMBER,
  HandlePost2MemberValidates = [
    ],
  HandlePost2Member = app_util:declare_api_validate(<<"post">>,<<Path1/binary,"/",PathMember/binary>>,HandlePost2MemberValidates),

  HandleDelete = app_util:declare_api_validate(<<"delete">>,Path,[]),

  Apis = [
    HandleGet, 
    HandlePut,
    HandleGet1,
    HandlePost1,
    HandlePost2Verify,
    HandlePost2Member,
    HandleDelete
  ],
 app_util:create_module_validates(Apis).

