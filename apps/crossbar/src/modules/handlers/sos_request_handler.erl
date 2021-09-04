-module(sos_request_handler).

-include("crossbar.hrl").

-export([
    calculate_color_type/1,
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