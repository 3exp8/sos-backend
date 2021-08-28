-module(device_doc).
-behaviour(sumo_doc).
-export([
  sumo_schema/0, 
  sumo_sleep/1, 
  sumo_wakeup/1
]).

-export([
         create_schema/0,
         delete_schema/0,
         init_data/0
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
    client_id =>  maps:get(client_id, Doc, <<>>),
    status => maps:get(status, Doc, <<>>),
    account_id  => maps:get(account_id, Doc, <<>>),
    account_scope => maps:get(account_scope, Doc, <<>>),
    app_id   => maps:get(app_id, Doc, <<>>),
    push_id => maps:get(push_id, Doc, <<>>),
    os_type => maps:get(os_type, Doc, <<>>),
    env => maps:get(env, Doc, <<>>),
    %mqtt_password => maps:get(mqtt_password_id, Doc, <<>>),
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
    client_id =>  maps:get(client_id, Info, <<>>),
    status => maps:get(status, Info, <<>>),
    account_id => maps:get(account_id, Info, <<>>),
    account_scope => maps:get(account_scope, Info, <<>>),
    app_id   => maps:get(app_id, Info, <<>>),
    push_id => maps:get(push_id, Info, <<>>),
    os_type => maps:get(os_type, Info, <<>>),
    env => maps:get(env, Info, <<>>),
    %mqtt_password_id => maps:get(mqtt_password, Device, <<>>),
    created_by => maps:get(created_by, Info, <<>>),
    created_time => maps:get(created_time, Info, DefaultTime),
    updated_by => maps:get(updated_by, Info, <<>>),
    updated_time => maps:get(updated_time, Info, DefaultTime)
   }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
                            sumo:new_field(id , binary, [not_null, id]),
                            sumo:new_field(client_id , binary, [not_null]),
                            sumo:new_field(status, binary, [not_null]),
                            sumo:new_field(account_id , binary, [not_null]),
                            sumo:new_field(account_scope , binary, [not_null]),
                            sumo:new_field(app_id  , binary, [not_null]),
                            sumo:new_field(push_id , binary),
                            sumo:new_field(os_type, binary, [not_null]),
                            sumo:new_field(env , binary),
                            %sumo:new_field(mqtt_password_id , binary, [not_null]),
                            sumo:new_field(created_by , binary),
                            sumo:new_field(created_time , datetime),
                            sumo:new_field(updated_by , binary),
                            sumo:new_field(updated_time , datetime)
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

init_data() ->
  Uuid = zt_util:get_uuid(),
  Id = <<"device", Uuid/binary>>,
  CreateTime = zt_datetime:get_now(),
  AccountId = <<"customerddfc2382b892fae1d4857b8cae670e73">>,
  PushId = <<"dWX36eUHGI4:APA91bGzxIfhygwsLrhAUGlgRE4y7COEMQC2UFx57KZ7ryFcRqhiYvuLzLvcI69gLleMnNG3-uGRnDJ_4f6lzxXyZVTSvJLCttWC6BbQae1PQH46vJ98bURK3HG89WziJcpfx8HkJ6Hs">>,
  Doc = #{id => Id,
          status => <<"active">>,
          account_id => AccountId,
          account_scope => <<"CUSTOMER">>,
          app_id => <<>>,
          push_id => PushId,
          os_type => <<>>,
          env => <<>>,
          %mqtt_password => <<>>,
          created_by => AccountId,
          created_time => CreateTime,
          updated_by => AccountId,
          updated_time => CreateTime
         },
  device_db:save(Doc).