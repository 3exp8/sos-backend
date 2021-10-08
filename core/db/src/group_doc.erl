-module(group_doc).
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
    io:format("Doc: ~p~n",[Doc]),
    lager:debug("Doc: ~p~n",[Doc]),
	#{
        id =>  maps:get(id, Doc, <<>>),
        name =>  maps:get(name, Doc, <<>>),
        type =>  maps:get(type, Doc, <<>>),
        location =>  maps:get(location, Doc, <<"0,0">>),
        avatar =>  maps:get(avatar, Doc, <<>>),
        address_info =>  maps:get(address_info, Doc, #{}),
        contact_info =>  maps:get(contact_info, Doc, #{}),
        detail_info =>  maps:get(detail_info, Doc, #{}),
        admin_id =>  maps:get(admin_id, Doc, <<>>),
        members =>  maps:get(members, Doc, []),
        verify_status =>  maps:get(verify_status, Doc, <<>>),
        verify_info =>  maps:get(verify_info, Doc, #{}),
        description =>  maps:get(description, Doc, <<>>),
        distance => maps:get(x_distance, Doc, 1.0),
        created_by => maps:get(created_by, Doc, <<>>),
        created_time => maps:get(created_time, Doc, DefaultTime),
        updated_by => maps:get(updated_by, Doc, <<>>),
        updated_time => maps:get(updated_time, Doc, DefaultTime)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(info()) -> sumo:doc().
sumo_sleep(Info) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, Info, <<>>),
        name =>  maps:get(name, Info, <<>>),
        type =>  maps:get(type, Info, <<>>),
        location =>  maps:get(location, Info, <<"0,0">>),
        avatar =>  maps:get(avatar, Info, <<>>),
        address_info =>  maps:get(address_info, Info, #{}),
        contact_info =>  maps:get(contact_info, Info, #{}),
        detail_info =>  maps:get(detail_info, Info, #{}),
        admin_id =>  maps:get(admin_id, Info, <<>>),
        members =>  maps:get(members, Info, []),
        verify_status =>  maps:get(verify_status, Info, <<>>),
        verify_info =>  maps:get(verify_info, Info, #{}),
        description =>  maps:get(description, Info, <<>>),
        created_by => maps:get(created_by, Info, <<>>),
        created_time => maps:get(created_time, Info, DefaultTime),
        updated_by => maps:get(updated_by, Info, <<>>),
        updated_time => maps:get(updated_time, Info, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
        sumo:new_field(id, binary, [not_null, id]),
        sumo:new_field(name, string),
        sumo:new_field(type, binary),
        sumo:new_field(location, geo_point),
        sumo:new_field(address_info, object),
        sumo:new_field(avatar, binary),
        sumo:new_field(contact_info, object),
        sumo:new_field(detail_info, object),
        sumo:new_field(admin_id, binary),
        sumo:new_field(members, object_list),
        sumo:new_field(verify_status, binary),
        sumo:new_field(verify_info, object),
        sumo:new_field(description, string),
        sumo:new_field(created_by, binary),
        sumo:new_field(created_time, datetime),
        sumo:new_field(updated_by, binary),
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