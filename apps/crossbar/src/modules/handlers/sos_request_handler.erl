-module(sos_request_handler).

-include("crossbar.hrl").

-export([
    calculate_color_type/1,
    get_supporter_info/2,
    get_suggester_info/1,
    maybe_update_support_status/4,
    get_target_type/2,
    validate_suggest_target_type/2,
    validate_suggest_target_id/2,
    validate_requester_type/2,
    validate_share_phone_number_update/2,
    validate_share_phone_number/2,
    validate_requester_type_update/2,
    validate_search_lat/2,
    validate_search_long/2
]).

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

get_requester_info(_Id, ?REQUESTER_TYPE_GUEST) -> #{};
get_requester_info(Id, ?REQUESTER_TYPE_USER) ->
    case user_db:find(Id) of
        notfound ->
            {error,customer_notfound};
        CustomerInfo -> maps:with([id,first_name, last_name, phone_number],CustomerInfo)
    end;

get_requester_info(Id, ?REQUESTER_TYPE_GROUP) ->
    %%TODO
    case group_db:find(Id) of 
        notfound -> {error,group_notfound};
        GroupInfo -> maps:with([id,type,name],GroupInfo)
    end.

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
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  case api_util:check_val(Context, Key, Val) of 
    Context -> 
        validate_requester_type_value(Key, Val, Context);
    ErrorContext -> 
        ErrorContext
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