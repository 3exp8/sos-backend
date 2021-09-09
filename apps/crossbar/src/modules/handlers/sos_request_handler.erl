-module(sos_request_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    calculate_color_type/1,
    get_supporter_info/2,
    get_suggester_info/1,
    maybe_update_support_status/4,
    maybe_add_bookmarks/2,
    maybe_remove_bookmarks/3,
    get_target_type/2,
    validate_bookmarker_type/2,
    validate_bookmarker_id/2,
    validate_suggest_target_type/2,
    validate_suggest_target_id/2,
    validate_requester_type/2,
    validate_share_phone_number_update/2,
    validate_share_phone_number/2,
    validate_requester_type_update/2,
    validate_search_lat/2,
    validate_search_long/2,
    change_request_status/2,
    change_task_status/2,
    maybe_update_request_status/2,
    validate_update_status/2,
    get_requester_info/1,
    get_requester_info/2,
    deformat_sorts/3,
    build_search_conditions/2,
    find_bookmark/3,
    maybe_filter_bookmark/3,
    maybe_filter_bookmark_by_group/2
]).

not_found_user_bookmark(SosRequestInfo) -> 
    maps:merge(SosRequestInfo, #{
        is_bookmarked => false
    }).
not_found_group_bookmark(SosRequestInfo) -> 
    maps:merge(SosRequestInfo, #{
        is_group_bookmarked => false
    }).
maybe_filter_bookmark(_,<<>>, SosRequestInfo) -> 
    not_found_user_bookmark(SosRequestInfo);
maybe_filter_bookmark(_,undefined, SosRequestInfo) -> maybe_filter_bookmark(ignore,<<>>, SosRequestInfo);
maybe_filter_bookmark(ObjecType, ObjectId, #{bookmarks := Bookmarks} = SosRequestInfo) -> 
    case find_bookmark(Bookmarks, ObjecType, ObjectId) of 
        {true, BookmarkInfo} ->
                maps:merge(SosRequestInfo, #{
                    is_bookmarked => true,
                    bookmark_info => BookmarkInfo
                });
        false -> maybe_filter_bookmark(ignore,<<>>, SosRequestInfo)
    end.

maybe_filter_bookmark_by_group(Groups, #{bookmarks := Bookmarks} = SosRequestInfo) ->
    GroupBookmarks = 
    lists:filtermap(fun(#{id := GroupId}) ->
        case find_bookmark(Bookmarks, ?OBJECT_TYPE_GROUP, GroupId) of 
            {true, BookmarkInfo} ->
                    NewSosRequestInfo = 
                        maps:merge(SosRequestInfo, #{
                            is_group_bookmarked => true,
                            bookmark_info => BookmarkInfo
                        }),
                    {true, NewSosRequestInfo};
            false -> fasle
        end

    end,Groups),
    case length(GroupBookmarks) of 
        0 -> not_found_group_bookmark(SosRequestInfo);
        _ -> 
            maps:merge(SosRequestInfo, #{
                is_group_bookmarked => true,
                group_bookmarks => GroupBookmarks
            })
    end.
    
find_bookmark([], Type, Id) -> false;

find_bookmark([Info|OtherBookmars], Type, Id) -> 
      case Info of 
      #{
        <<"id">> := Id 
      } ->  {true, Info};
      _ -> find_bookmark(OtherBookmars, Type, Id)
    end.

change_request_status(?SOS_REQUEST_STATUS_OPEN,?SOS_REQUEST_STATUS_VERIFIED) -> ?SOS_REQUEST_STATUS_VERIFIED;
change_request_status(?SOS_REQUEST_STATUS_REOPEN,?SOS_REQUEST_STATUS_VERIFIED) -> ?SOS_REQUEST_STATUS_VERIFIED;

change_request_status(?SOS_REQUEST_STATUS_OPEN,?SOS_REQUEST_STATUS_ACCEPTED) -> ?SOS_REQUEST_STATUS_ACCEPTED;
change_request_status(?SOS_REQUEST_STATUS_VERIFIED,?SOS_REQUEST_STATUS_ACCEPTED) -> ?SOS_REQUEST_STATUS_ACCEPTED;
change_request_status(?SOS_REQUEST_STATUS_REOPEN,?SOS_REQUEST_STATUS_ACCEPTED) -> ?SOS_REQUEST_STATUS_ACCEPTED;


change_request_status(?SOS_REQUEST_STATUS_ACCEPTED,?SOS_REQUEST_STATUS_EXECUTING) -> ?SOS_REQUEST_STATUS_EXECUTING;

change_request_status(?SOS_REQUEST_STATUS_EXECUTING,?SOS_REQUEST_STATUS_RESOLVED) -> ?SOS_REQUEST_STATUS_RESOLVED;

change_request_status(?SOS_REQUEST_STATUS_OPEN,?SOS_REQUEST_STATUS_REJECTED) -> ?SOS_REQUEST_STATUS_REJECTED;
change_request_status(?SOS_REQUEST_STATUS_REOPEN,?SOS_REQUEST_STATUS_REJECTED) -> ?SOS_REQUEST_STATUS_REJECTED;


change_request_status(?SOS_REQUEST_STATUS_RESOLVED,?SOS_REQUEST_STATUS_REOPEN) -> ?SOS_REQUEST_STATUS_REOPEN;
change_request_status(?SOS_REQUEST_STATUS_REJECTED,?SOS_REQUEST_STATUS_REOPEN) -> ?SOS_REQUEST_STATUS_REOPEN;

change_request_status(CurrentStatus,NewStatus) ->
    lager:debug("change_request_status ignore updateing status ~p to ~p ~n",[CurrentStatus,NewStatus]),
    CurrentStatus.

change_task_status(?SOS_TASK_STATUS_OPEN,?SOS_TASK_STATUS_EXECUTING) -> ?SOS_TASK_STATUS_EXECUTING;
change_task_status(?SOS_TASK_STATUS_PENDING,?SOS_TASK_STATUS_EXECUTING) -> ?SOS_TASK_STATUS_EXECUTING;
change_task_status(?SOS_TASK_STATUS_CANCELED,?SOS_TASK_STATUS_EXECUTING) -> ?SOS_TASK_STATUS_EXECUTING;

change_task_status(?SOS_TASK_STATUS_EXECUTING,?SOS_TASK_STATUS_RESOLVED) -> ?SOS_TASK_STATUS_RESOLVED;

change_task_status(?SOS_TASK_STATUS_OPEN,?SOS_TASK_STATUS_PENDING) -> ?SOS_TASK_STATUS_PENDING;
change_task_status(?SOS_TASK_STATUS_EXECUTING,?SOS_TASK_STATUS_PENDING) -> ?SOS_TASK_STATUS_PENDING;

change_task_status(?SOS_TASK_STATUS_OPEN,?SOS_TASK_STATUS_CANCELED) -> ?SOS_TASK_STATUS_CANCELED;
change_task_status(?SOS_TASK_STATUS_PENDING,?SOS_TASK_STATUS_CANCELED) -> ?SOS_TASK_STATUS_CANCELED;
change_task_status(?SOS_TASK_STATUS_EXECUTING,?SOS_TASK_STATUS_CANCELED) -> ?SOS_TASK_STATUS_CANCELED;


change_task_status(CurrentStatus,NewStatus) ->
    lager:debug("change_task_status ignore updateing status ~p to ~p ~n",[CurrentStatus,NewStatus]),
    CurrentStatus.

maybe_add_bookmarks(undefined, BookmarkInfo) -> maybe_add_bookmarks([], BookmarkInfo);
maybe_add_bookmarks(CurrentBookmarks, #{
    bookmarker_type := BookmarkerType,
    bookmarker_id := BookmarkerId
} = BookmarkInfo) ->
    IsExist = 
        lists:any(fun(#{
            <<"bookmarker_type">> := BookmarkerTypeDb,
            <<"bookmarker_id">> := BookmarkerIdDb
        }) -> 
            case {BookmarkerType, BookmarkerId} of 
                {BookmarkerTypeDb, BookmarkerIdDb} -> true;
                _ -> false 
            end
        end, CurrentBookmarks),
    case IsExist of 
        true -> CurrentBookmarks;
        _ -> [BookmarkInfo|CurrentBookmarks]
    end.


maybe_remove_bookmarks(undefined,TargetType, TargetId) -> maybe_remove_bookmarks([],TargetType, TargetId);
maybe_remove_bookmarks(CurrentBookmarks, TargetType, TargetId) ->
    lists:filtermap(fun(#{
        <<"bookmarker_type">> := BookmarkerType, 
        <<"bookmarker_id">> := BookmarkerId 
    } = BookmarkInfo) -> 
        case {BookmarkerType, BookmarkerId} of 
            {TargetType, TargetId} -> false;
            _ -> {true, BookmarkInfo}
        end
    end,CurrentBookmarks).

-spec validate_bookmarker_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_bookmarker_type(ReqJson, Context) ->
  Key = <<"bookmarker_type">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  case api_util:check_val(Context, Key, Val) of 
    Context -> 
    validate_bookmarker_type_value(Key, Val, Context);
    ErrorContext -> 
        ErrorContext
  end.

-spec validate_bookmarker_type_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_bookmarker_type_value(Key, Val, Context) ->
    case lists:member(Val, ?SUGGEST_TARGET_TYPES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?SUGGEST_TARGET_TYPES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

validate_bookmarker_id(ReqJson, Context) ->
    Key = <<"bookmarker_id">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    api_util:check_val(Context, Key, Val).

validate_update_status(ReqJson, Context) ->
    Key = <<"status">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    case api_util:check_val(Context, Key, Val) of 
        Context -> 
            case lists:member(Val, ?SOS_REQUEST_STATUS_LIST) of
                true -> Context;
                _ ->
                    Vals = zt_util:arr_to_str(?SOS_REQUEST_STATUS_LIST),
                    api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
            end;
        ErrorContext -> ErrorContext
    end.

%-spec calculate_color_type([]) -> map().
calculate_color_type([]) -> #{};

calculate_color_type(SupportTypes) -> 
lager:debug("calculate_color_type SupportTypes: ~p~n",[SupportTypes]),
Types = 
    lists:map(fun(#{type := Type}) -> 
        Type
    end,SupportTypes),
SupportTypeDb = support_type_db:find_by_conditions([{type,'in',Types}],[],100,0),
Colors = 
    lists:map(fun(#{color_info := #{ <<"color">> := ColorDb}}) -> 
        ColorDb
    end,SupportTypeDb),
ColorInfo = configuration_handler:get_high_priority_color(Colors),
lager:debug("calculate_color_type ColorInfo: ~p~n",[ColorInfo]),
ColorInfo.

% Requester update status or
% Operator update status
maybe_update_request_status(#{
    status := CurrentStatus,
    status_history := StatusHistoryDb,
    requester_info := RequesterInfoRaw
} = SosRequestInfo, Context) ->
    ReqJson =  cb_context:req_json(Context),
    UserId =  cb_context:user_id(Context),
    RequesterInfo = zt_util:map_keys_to_atom(RequesterInfoRaw),
    RequesterId = maps:get(id,RequesterInfo,<<>>),
    Role = cb_context:role(Context),
    case check_permission_update_request_status(RequesterId,UserId, Role) of 
        false -> {error,forbidden};
        true -> 
            NewStatus = wh_json:get_value(<<"status">>, ReqJson,<<>>),
            case change_request_status(CurrentStatus, NewStatus) of 
                CurrentStatus -> {warning, status_nochange};
                NewStatus ->
                    #{
                       first_name := FirstName,
                       last_name := LastName
                    } = user_db:find(UserId),
                    NewStatusHistory = add_status_history(#{
                        user_id => UserId,
                        first_name => FirstName,
                        last_name => LastName,
                        status => NewStatus,
                        note => wh_json:get_value(<<"note">>, ReqJson,<<>>),
                        time => zt_datetime:get_now()
                    }, StatusHistoryDb),
                NewSosRequestInfo = 
                        maps:merge(SosRequestInfo, #{
                            status => NewStatus,
                            status_history => NewStatusHistory
                        }),
                sos_request_db:save(NewSosRequestInfo),
                {success,NewSosRequestInfo}
            end
    end.
add_status_history(StatusInfo, undefined) -> 
    add_status_history(StatusInfo, []); 

add_status_history(StatusInfo, CurrentStatusHistory) when is_list(CurrentStatusHistory) -> 
    [StatusInfo|CurrentStatusHistory].

%todo: review role
check_permission_update_request_status(RequesterId, RequesterId, _) -> true;
check_permission_update_request_status(_, _, ?USER_ROLE_OPERATOR) -> true;
check_permission_update_request_status(_, _, ?USER_ROLE_ADMIN) -> true;
check_permission_update_request_status(_, _, _) -> false.

% Supporter update their task
maybe_update_support_status(Type, Id, _SosRequestInfo, <<>>) -> 
    lager:debug("Do not update support status: Type: ~p, Id: ~p~n",[Type, Id]),
    {error,no_change};

maybe_update_support_status(Type, Id, SosRequestInfo, NewSupportStatus) -> 
        
    Supporters = maps:get(supporters, SosRequestInfo, []),
    NewSupporters = 
        lists:map(fun(SupportInfo) -> 
                    case SupportInfo of 
                        #{
                            <<"type">> := Type,
                            <<"id">> := Id,
                            <<"status">> := CurrentSupportStatus 
                        } ->
                            maps:merge(SupportInfo, #{
                                <<"status">> => change_task_status(CurrentSupportStatus, NewSupportStatus),
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

get_supporter_info(?REQUESTER_TYPE_GROUP, Id) -> 
   case  group_db:find(Id) of 
   notfound -> {error,notfound};
  #{
        name := Name,
        contact_info := ContactInfo
  } -> 
    #{
        id => Id,
        type => ?REQUESTER_TYPE_GROUP,
        name => Name,
        contact_info => ContactInfo
    }
end;

get_supporter_info(?REQUESTER_TYPE_USER, Id) -> 
   case user_db:find(Id) of 
   notfound -> {error,notfound};
  #{
        first_name := FirstName,
        last_name := LastName,
        phone_number := PhoneNumber
  } -> 
        #{
            id => Id,
            type => ?REQUESTER_TYPE_USER,
            name => FirstName,
            contact_info => #{
                first_name => FirstName,
                last_name => LastName,
                phone_number => PhoneNumber
            }
        };
    Other -> 
        lager:debug("other: ~p~n",[Other]),
        {error,other}
end;

get_supporter_info(_, _) -> {error, not_support}.

get_target_type(?OBJECT_TYPE_USER = TargetType,Id) -> 
    case user_db:find(Id) of 
        notfound -> 
            {error,notfound_user};
        #{
            first_name := FirstName,
            last_name := LastName
        } -> 
            {TargetType, Id, zt_util:full_name(FirstName, LastName)}
    end;

get_target_type(?OBJECT_TYPE_GROUP = TargetType,GroupId) -> 
    case group_db:find(GroupId) of 
        notfound -> 
            {error,notfound_group};
        #{
            name := GroupName
        } -> 
            {TargetType, GroupId,GroupName}
    end.

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

get_requester_info(Context) ->  
    ReqJson = cb_context:req_json(Context),
    get_requester_info(
        wh_json:get_value(<<"requester_type">>, ReqJson, <<>>),
        Context
    ).

get_requester_info(<<>>, Context) ->  
    case cb_context:user_id(Context) of 
        undefined -> 
            get_requester_info(?REQUESTER_TYPE_GUEST, Context);
        <<>> -> 
            get_requester_info(?REQUESTER_TYPE_GUEST, Context);
        _ -> 
            get_requester_info(?REQUESTER_TYPE_USER,Context)
    end;

get_requester_info(?REQUESTER_TYPE_USER, Context) ->
    UserId = cb_context:user_id(Context),
    case user_db:find(UserId) of 
        notfound -> {error, notfound_user};
        UserInfo -> 
            {?REQUESTER_TYPE_USER,maps:with([id,first_name, last_name, phone_number],UserInfo)}
    end;

get_requester_info(?REQUESTER_TYPE_GROUP, Context) ->
    ReqJson = cb_context:req_json(Context),
    RequesterId = wh_json:get_value(<<"requester_id">>, ReqJson, <<>>),
    case group_db:find(RequesterId) of 
        notfound -> {error, notfound_group};
        GroupInfo -> 
            {?REQUESTER_TYPE_GROUP,maps:with([id,type,name],GroupInfo)}
    end;

get_requester_info(_, Context) ->  {?REQUESTER_TYPE_GUEST,#{}}.

get_suggester_info(Id) ->
    case user_db:find(Id) of
        notfound ->
            {error,user_notfound};
        #{
            first_name := FirstName,
            last_name := LastName
        } -> #{
            suggester_id => Id,
            suggester_name => zt_util:full_name(FirstName, LastName)
        }
    end.

-spec validate_suggest_target_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_suggest_target_type(ReqJson, Context) ->
  Key = <<"target_type">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  case api_util:check_val(Context, Key, Val) of 
    Context -> 
        validate_suggest_taget_type_value(Key, Val, Context);
    ErrorContext -> 
        ErrorContext
  end.

-spec validate_suggest_taget_type_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_suggest_taget_type_value(Key, Val, Context) ->
    case lists:member(Val, ?SUGGEST_TARGET_TYPES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?SUGGEST_TARGET_TYPES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

validate_suggest_target_id(ReqJson, Context) ->
    Key = <<"target_id">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    api_util:check_val(Context, Key, Val).

-spec validate_requester_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_requester_type(ReqJson, Context) ->
  Key = <<"requester_type">>,
  case wh_json:get_value(Key, ReqJson, <<>>) of
    <<>> -> Context;
    Val -> 
        validate_requester_type_value(Key, Val, Context)
  end.

-spec validate_requester_type_update(api_binary(), cb_context:context()) -> cb_context:context().
validate_requester_type_update(ReqJson, Context) ->
  Key = <<"requester_type">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  validate_requester_type_value(Key, Val, Context).

-spec validate_share_phone_number(api_binary(), cb_context:context()) -> cb_context:context().
validate_share_phone_number(ReqJson, Context) ->
  Key = <<"share_phone_number">>,
  case wh_json:get_value(Key, ReqJson, <<>>) of 
    <<>> -> 
        Context;
    Val -> 
        validate_share_phone_number_value(Key, Val, Context)
  end.

-spec validate_share_phone_number_update(api_binary(), cb_context:context()) -> cb_context:context().
validate_share_phone_number_update(ReqJson, Context) ->
  Key = <<"share_phone_number">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  validate_share_phone_number_value(Key, Val, Context).

-spec validate_requester_type_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_requester_type_value(Key, Val, Context) ->
    case lists:member(Val, ?REQUESTER_TYPES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?REQUESTER_TYPES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

-spec validate_share_phone_number_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_share_phone_number_value(Key, Val, Context) ->
    case lists:member(Val, ?SHARE_PHONE_NUMBER_TYPES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?SHARE_PHONE_NUMBER_TYPES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

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