-module(support_type_doc).

-behaviour(sumo_doc).

-export([
    sumo_schema/0, 
    sumo_sleep/1, 
    sumo_wakeup/1
]).

-export([
            create_schema/0,
            delete_schema/0
        ]).

-opaque info() :: map().

-export_type([info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> info().
sumo_wakeup(Doc) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
	#{
        id =>  maps:get(id, Doc),
        type =>  maps:get(type, Doc, <<>>),
        name =>  maps:get(name, Doc, <<>>),
        unit =>  maps:get(unit, Doc, <<>>),
        priority =>  maps:get(priority, Doc, 1),
        color_type =>  maps:get(color_type, Doc, #{}),
        target_types =>  maps:get(target_types, Doc, <<>>),
        created_by_id => maps:get(created_by_id, Doc, <<>>),
        created_time => maps:get(created_time, Doc, DefaultTime),
        updated_by_id => maps:get(updated_by_id, Doc, <<>>),
        updated_time => maps:get(updated_time, Doc, DefaultTime)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(info()) -> sumo:doc().
sumo_sleep(Info) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, Info),
        type =>  maps:get(type, Info, <<>>),
        name =>  maps:get(name, Info, <<>>),
        unit =>  maps:get(unit, Info, <<>>),
        priority =>  maps:get(priority, Info, 1),
        color_type =>  maps:get(color_type, Info, #{}),
        target_types =>  maps:get(target_types, Info, <<>>),
        created_by_id => maps:get(created_by_id, Info, <<>>),
        created_time => maps:get(created_time, Info, DefaultTime),
        updated_by_id => maps:get(updated_by_id, Info, <<>>),
        updated_time => maps:get(updated_time, Info, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
        sumo:new_field(id, binary, [not_null, id]),
        sumo:new_field(type, binary),
        sumo:new_field(name, string),
        sumo:new_field(unit, binary),
        sumo:new_field(priority, integer),
        sumo:new_field(color_type, object),
        sumo:new_field(target_types, object_list),
        sumo:new_field(created_by_id, binary),
        sumo:new_field(created_time, datetime),
        sumo:new_field(updated_by_id, binary),
        sumo:new_field(updated_time, datetime)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_schema() -> ok.
create_schema() ->
  sumo:create_schema(?MODULE).  
  
 -spec delete_schema() -> ok.
delete_schema() ->
  sumo:delete_schema(?MODULE).