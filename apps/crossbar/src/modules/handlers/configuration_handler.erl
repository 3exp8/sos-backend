-module(configuration_handler).

-include("crossbar.hrl").


-export([
    get_color_types/0,
    get_color_codes/0,
    get_color_type/1,
    get_high_priority_color/1,
    is_key_exist/1,
    validate_type/2,
    validate_group/2,
    validate_value/2, 
    validate_content_type/2
]).

is_key_exist( Key) ->
	case configuration_db:find_by_conditions([{key,Key}], [], 1, 0) of 
    [ConfigurationDb] when is_map(ConfigurationDb) ->
      true ;
    [] ->
      false ;
    Error ->
      lager:error("Configuration Can't Register. Maybe Database With This Key: ~p; Error: ~p ~n", [Key,Error]),
      throw(dberror)
  end.

-spec validate_type(api_binary(),cb_context:context())->cb_context:cb_context().
validate_type(ReqJson, Context) ->
	Type = wh_json:get_value(<<"type">>,ReqJson,<<>>),
  	case Type of
		<<>> ->
			  lager:debug("Required TYPE"),
			  api_util:validate_error(Context,<<"type">>,<<"required">>,<<"Field 'type' is required">>);
		_ -> Context
	end.

-spec validate_value(api_binary(),cb_context:context())->cb_context:cb_context().
validate_value(ReqJson, Context) ->
	Value = wh_json:get_value(<<"value">>,ReqJson,<<>>),
	case Value of 
		<<>> ->
				lager:debug("Required Value"),
				api_util:validate_error(Context,<<"value">>,<<"required">>,<<"Field 'value' is required">>);
		_ -> Context
	end.

-spec validate_group(api_binary(),cb_context:context()) -> cb_context:cb_context().
validate_group(ReqJson, Context) ->
	Group = wh_json:get_value(<<"group">>,ReqJson,<<>>),
	case Group of 
		<<>> -> 
			lager:debug("Required Group"),
			api_util:validate_error(Context,<<"group">>,<<"required">>,<<"Field 'group' is required">>);
		_ -> Context
	end.

-spec validate_content_type(api_binary(),cb_context:context())->cb_context:cb_context().
validate_content_type(ReqJson, Context) ->
	Value = wh_json:get_value(<<"content_type">>,ReqJson,<<>>),
	case Value of 
		<<>> ->
				lager:debug("Required Content Type"),
				api_util:validate_error(Context,<<"content_type">>,<<"required">>,<<"Field 'content_type' is required">>);
		_ -> Context
	end.

get_high_priority_color(ColorTypes) when is_list(ColorTypes) -> 
    Lists = get_color_types(),
    FilteredLists = 
        lists:filter(fun(#{color := Color}) -> 
            lists:member(Color, ColorTypes)
        end,Lists),
    Sorted =
        lists:usort(fun(#{priority := P1}, #{priority := P2}) -> 
            P1 < P2
        end,FilteredLists ++ [get_color_type(<<>>)]),
    lists:nth(1,Sorted);

get_high_priority_color(ColorType) -> get_high_priority_color([ColorType]).



-spec get_color_type(binary()) -> map().
get_color_type(<<>>) -> 
    color_type(<<"gray">>,1000);

get_color_type(Color) -> 
    Colors = get_color_types(),
    ColorsMap = zt_util:to_map_with_key(color,Colors),
    maps:get(Color, ColorsMap, get_color_type(<<>>)).



-spec get_color_types() -> [map()].
get_color_types() -> 
    [
        color_type(<<"red">>,1),
        color_type(<<"orange">>,2),
        color_type(<<"yellow">>,3),
        color_type(<<"green">>,4)
    ].

-spec get_color_codes() -> [binary()].
get_color_codes() -> 
    Types = get_color_types(),
    lists:map(fun(#{color := Color}) -> 
        Color
    end,Types).

-spec color_type(binary(),integer()) -> map().
color_type(Color, Priority) -> 
    color_type(Color, Priority, <<>>).

-spec color_type(binary(), integer(), binary()) -> map().
color_type(Color, Priority, Description) -> 
#{
    color => Color,
    priority => Priority,
    description => Description
}.