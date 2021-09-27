-module(sos_request_doc).
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
  #{
    id =>  maps:get(id, Doc, <<>>),
    type => zt_util:nvl(maps:get(type, Doc),<<>>),
    subject =>  maps:get(subject, Doc, <<>>),
    priority_type =>  maps:get(priority_type, Doc, <<>>),
    color_info =>  maps:get(color_info, Doc, #{}),
    support_types =>  maps:get(support_types, Doc, []),
    supporters =>  maps:get(supporters, Doc, []),
    suggest_info =>  maps:get(suggest_info, Doc, #{}),
    suggests =>  zt_util:nvl(maps:get(suggests, Doc, []),[]),
    bookmarks =>  maps:get(bookmarks, Doc, []),
    medias =>  maps:get(medias, Doc, []),
    location =>  maps:get(location, Doc, <<"0,0">>),
    distance => maps:get(x_distance, Doc, 0.0),
    description =>  maps:get(description, Doc, <<>>),
    address_info =>  maps:get(address_info, Doc, #{}),
    contact_info =>  maps:get(contact_info, Doc, #{}),
    share_phone_number => maps:get(share_phone_number, Doc, <<"private">>),
    requester_type => maps:get(requester_type, Doc, <<>>),
    requester_info => maps:get(requester_info, Doc, #{}),
    requester_object_status => maps:get(requester_object_status, Doc, []),
    verify_status => maps:get(verify_status, Doc, <<>>),
    status => maps:get(status, Doc, <<>>),
    status_history => maps:get(status_history, Doc, []),
    created_by => maps:get(created_by, Doc, <<>>),
    created_time => maps:get(created_time, Doc, <<>>),
    updated_by => maps:get(updated_by, Doc, <<>>),
    updated_time => maps:get(updated_time, Doc, <<>>)
   }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(info()) -> sumo:doc().
sumo_sleep(Info) ->
  DefaultTime = util_db:now_to_utc_binary({0,0,0}),
  #{
    id =>  maps:get(id, Info, <<>>),
    type => maps:get(type, Info, <<>>),
    subject =>  maps:get(subject, Info, <<>>),
    priority_type =>  maps:get(priority_type, Info, <<>>),
    color_info =>  maps:get(color_info, Info, #{}),
    support_types =>  maps:get(support_types, Info, []),
    supporters =>  maps:get(supporters, Info, []),
    suggest_info => maps:get(suggest_info, Info, #{}),
    suggests => maps:get(suggests, Info, []),
    bookmarks =>  maps:get(bookmarks, Info, []),
    medias =>  maps:get(medias, Info, []),
    location =>  maps:get(location, Info, <<"0,0">>),
    description =>  maps:get(description, Info, <<>>),
    address_info =>  maps:get(address_info, Info, #{}),
    contact_info =>  maps:get(contact_info, Info, #{}),
    requester_type => maps:get(requester_type, Info, <<>>),
    share_phone_number => maps:get(share_phone_number, Info, <<"private">>),
    requester_info => maps:get(requester_info, Info, #{}),
    requester_object_status => maps:get(requester_object_status, Info, []),
    verify_status => maps:get(verify_status, Info, <<>>),
    status => maps:get(status, Info, <<>>),
    status_history => maps:get(status_history, Info, []),
    created_by => maps:get(created_by, Info, <<>>),
    created_time => maps:get(created_time, Info, DefaultTime),
    updated_by => maps:get(updated_by, Info, <<>>),
    updated_time => maps:get(updated_time, Info, DefaultTime)
   }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
                            sumo:new_field(id, binary, [not_null, id]),
                            sumo:new_field(type, binary),
                            sumo:new_field(support_types, object_list),
                            sumo:new_field(supporters, object_list),
                            sumo:new_field(bookmarks, object_list),
                            sumo:new_field(suggest_info, object),
                            sumo:new_field(suggests, object_list),
                            sumo:new_field(medias, object_list),
                            sumo:new_field(description, string),
                            sumo:new_field(subject, string),
                            sumo:new_field(priority_type, binary),
                            sumo:new_field(color_info, object),
                            sumo:new_field(location, geo_point),
                            sumo:new_field(description, string),
                            sumo:new_field(address_info, object),
                            sumo:new_field(contact_info, object),
                            sumo:new_field(share_phone_number, binary),
                            sumo:new_field(requester_type, binary),
                            sumo:new_field(requester_info, object),
                            sumo:new_field(requester_object_status, object_list),
                            sumo:new_field(verify_status, binary),
                            sumo:new_field(status, binary),
                            sumo:new_field(status_history, object_list),
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