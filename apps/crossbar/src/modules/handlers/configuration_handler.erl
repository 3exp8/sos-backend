-module(configuration_handler).

-include("crossbar.hrl").


-export([
    get_color_types/0,
    get_color_codes/0,
    get_color_type/1,
    get_high_priority_color/1
]).

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