-module(configuration_handler).

-include("crossbar.hrl").


-export([
    get_color_types/0,
    get_color_codes/0,
    get_color_type/1
]).



-spec get_color_type(binary()) -> map().
get_color_type(<<>>) -> 
    color_type(<<"unknow">>);

get_color_type(Color) -> 
    Colors = get_color_types(),
    ColorsMap = zt_util:to_map_with_key(color,Colors),
    maps:get(Color, ColorsMap, get_color_type(<<>>)).



-spec get_color_types() -> [map()].
get_color_types() -> 
    [
        color_type(<<"red">>),
        color_type(<<"orange">>),
        color_type(<<"yellow">>),
        color_type(<<"green">>)
    ].

-spec get_color_codes() -> [binary()].
get_color_codes() -> 
    Types = get_color_types(),
    lists:map(fun(#{color := Color}) -> 
        Color
    end,Types).

-spec color_type(binary()) -> map().
color_type(Color) -> 
    color_type(Color, <<>>).

-spec color_type(binary(),binary()) -> map().
color_type(Color, Description) -> 
#{
    color => Color,
    description => Description
}.