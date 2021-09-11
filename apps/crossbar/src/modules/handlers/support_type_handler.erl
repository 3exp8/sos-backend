-module(support_type_handler).

-include("crossbar.hrl").

-export([
    validate_type/2,
    validate_name/2,
    validate_color_type/2
]).



-spec validate_color_type(api_binary(), cb_context:context()) -> cb_context:context().
validate_color_type(ReqJson, Context) ->
  Key = <<"color_type">>,
  Val = wh_json:get_value(Key, ReqJson, <<>>),
  case api_util:check_val(Context, Key, Val) of 
    Context -> 
    validate_color_type_value(Key, Val, Context);
    ErrorContext -> 
        ErrorContext
  end.

-spec validate_color_type_value(binary(), binary(), cb_context:context()) -> cb_context:context().
validate_color_type_value(Key, Val, Context) ->
    ColorTypes = configuration_handler:get_color_codes(),
    case lists:member(Val, ColorTypes) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(ColorTypes),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

validate_type(ReqJson, Context) ->
    Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"type">>, Type).

validate_name(ReqJson, Context) ->
    Val = wh_json:get_value(<<"name">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"name">>, Val).