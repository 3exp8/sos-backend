-module(news_doc).

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
        subject =>  maps:get(subject, Doc, <<>>),
        content =>  maps:get(content, Doc, <<>>),
        status =>  maps:get(status, Doc, <<>>),
        medias =>  maps:get(medias, Doc, []),
        target_type =>  maps:get(target_type, Doc, <<>>),
        target_id =>  maps:get(target_id, Doc, <<>>),
        detail_info =>  maps:get(detail_info, Doc, #{}),
        published_by_name => maps:get(published_by_name, Doc, <<>>),
        published_by_id => maps:get(published_by_id, Doc, <<>>),
        published_time => maps:get(published_time, Doc, DefaultTime),
        created_by_name => maps:get(created_by_name, Doc, <<>>),
        created_by_id => maps:get(created_by_id, Doc, <<>>),
        created_time => maps:get(created_time, Doc, DefaultTime),
        updated_by_name => maps:get(updated_by_name, Doc, <<>>),
        updated_by_id => maps:get(updated_by_id, Doc, <<>>),
        updated_time => maps:get(updated_time, Doc, DefaultTime)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(info()) -> sumo:doc().
sumo_sleep(Info) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, Info),
        subject =>  maps:get(subject, Info, <<>>),
        content =>  maps:get(content, Info, <<>>),
        status =>  maps:get(status, Info, <<>>),
        medias =>  maps:get(medias, Info, []),
        target_type =>  maps:get(target_type, Info, <<>>),
        target_id =>  maps:get(target_id, Info, <<>>),
        detail_info =>  maps:get(detail_info, Info, #{}),
        published_by_name => maps:get(published_by_name, Info, <<>>),
        published_by_id => maps:get(published_by_id, Info, <<>>),
        published_time => maps:get(published_time, Info, DefaultTime),
        created_by_name => maps:get(created_by_name, Info, <<>>),
        created_by_id => maps:get(created_by_id, Info, <<>>),
        created_time => maps:get(created_time, Info, DefaultTime),
        updated_by_name => maps:get(updated_by_name, Info, <<>>),
        updated_by_id => maps:get(updated_by_id, Info, <<>>),
        updated_time => maps:get(updated_time, Info, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
        sumo:new_field(id, binary, [not_null, id]),
        sumo:new_field(subject, string),
        sumo:new_field(content, string),
        sumo:new_field(status, binary), %pending, active, inactive
        sumo:new_field(medias, object_list),
        sumo:new_field(target_type, binary),
        sumo:new_field(target_id, binary),
        sumo:new_field(detail_info, object),
        sumo:new_field(published_by_name, string),
        sumo:new_field(published_by_id, binary),
        sumo:new_field(published_time, datetime),
        sumo:new_field(created_by_name, string),
        sumo:new_field(created_by_id, binary),
        sumo:new_field(created_time, datetime),
        sumo:new_field(updated_by_name, string),
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