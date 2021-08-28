-module(support_trans_doc).
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
    id =>  maps:get(id, Doc, <<>>),
    sos_request_id =>  maps:get(sos_request_id, Doc, <<>>),
    requester_info => maps:get(requester_info, Doc, #{}),
    supporter_info =>  maps:get(supporter_info, Doc, #{}),
    support_list =>  maps:get(support_list, Doc, []),
    support_time => maps:get(support_time, Doc, DefaultTime),
    medias =>  maps:get(medias, Doc, []),
    description =>  maps:get(description, Doc, <<>>),
    status => maps:get(status, Doc, <<>>),
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
    sos_request_id =>  maps:get(sos_request_id, Info, <<>>),
    requester_info => maps:get(requester_info, Info, #{}),
    supporter_info =>  maps:get(supporter_info, Info, #{}),
    support_list =>  maps:get(support_list, Info, []),
    support_time => maps:get(support_time, Info, DefaultTime),
    medias =>  maps:get(medias, Info, []),
    description =>  maps:get(description, Info, <<>>),
    status => maps:get(status, Info, <<>>),
    created_by => maps:get(created_by, Info, <<>>),
    created_time => maps:get(created_time, Info, DefaultTime),
    updated_by => maps:get(updated_by, Info, <<>>),
    updated_time => maps:get(updated_time, Info, DefaultTime)
   }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
                            sumo:new_field(id, binary, [not_null, id]),
                            sumo:new_field(sos_request_id, binary),
                            sumo:new_field(requester_info, object),
                            sumo:new_field(supporter_info, object),
                            sumo:new_field(support_list, object_list),
                            sumo:new_field(support_time, datetime),
                            sumo:new_field(medias, object_list),
                            sumo:new_field(description, string),
                            sumo:new_field(status, binary),
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