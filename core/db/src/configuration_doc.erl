-module(configuration_doc).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-export([id/1,account_id/1,restaurant_id/1,group/1
        ,type/1,content_type/1,value/1,key/1]).

-export([
            create_schema/0,
            delete_schema/0
        ]).


-opaque configuration():: map().

-type id()::binary().
-type account_id()::binary().
-type restaurant_id()::binary().
-type type()::binary().
-type group()::binary().
-type key()::binary().
-type value() :: binary().
-type content_type() ::binary().

-export_type([configuration/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> configuration().
sumo_wakeup(Doc) ->
	#{
        id =>  maps:get(id, Doc, <<>>),
        account_id   => maps:get(account_id, Doc, <<>>),
        restaurant_id => maps:get(restaurant_id, Doc, <<>>),
        type => maps:get(type, Doc, <<>>),
        name => maps:get(name, Doc, <<>>),
        group => maps:get(group, Doc, <<>>),
        key => maps:get(key, Doc, <<>>),
        value => maps:get(value, Doc, <<>>),
        value_obj => maps:get(value_obj, Doc, #{}),
        content_type => maps:get(content_type, Doc, <<>>),
        order => maps:get(order, Doc, 1),
        created_by => maps:get(created_by_id, Doc, <<>>),
        updated_by => maps:get(updated_by_id, Doc, <<>>),
        created_time_dt => maps:get(created_time_dt, Doc, <<>>),
        updated_time_dt => maps:get(updated_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(configuration()) -> sumo:doc().
sumo_sleep(Config) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, Config, <<>>),
        account_id   => maps:get(account_id, Config, <<>>),
        restaurant_id => maps:get(restaurant_id, Config, <<>>),
        type => maps:get(type, Config, <<>>),
        name => maps:get(name, Config, <<>>),
        group => maps:get(group, Config, <<>>),
        key => maps:get(key, Config, <<>>),
        value => maps:get(value, Config, <<>>),
        value_obj => maps:get(value_obj, Config, #{}),
        order => maps:get(order, Config, 1),
        content_type => maps:get(content_type, Config, <<>>),
        created_by => maps:get(created_by_id, Config, <<>>),
        updated_by => maps:get(updated_by_id, Config, <<>>),
        created_time_dt => maps:get(created_time_dt, Config, DefaultTime),
        updated_time_dt => maps:get(updated_time_dt, Config, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id, binary, [not_null, id]),
    sumo:new_field(account_id, binary, [ not_null]),
    sumo:new_field(restaurant_id, binary, [ not_null]),
    sumo:new_field(type, binary, [ not_null]),
    sumo:new_field(name, string, [ not_null]),
    sumo:new_field(group, binary, [ not_null]),
    sumo:new_field(key, binary, [ not_null]),
    sumo:new_field(value, binary, [ not_null]), %% TODO: array
    sumo:new_field(value_obj, object, [ not_null]), 
    sumo:new_field(content_type, binary), %% TODO: array
    sumo:new_field(order, integer),
    sumo:new_field(created_by_id , binary),
    sumo:new_field(created_time_dt , datetime),
    sumo:new_field(updated_by_id , binary),
    sumo:new_field(updated_time_dt , datetime)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_schema() -> ok.
create_schema() ->  sumo:create_schema(?MODULE).  

 -spec delete_schema() -> ok.
delete_schema() ->  sumo:delete_schema(?MODULE).   
  
-spec id(configuration())->id().
id(#{id:=Id}) -> Id.

-spec account_id(configuration())->account_id().
account_id(#{account_id:=Account_id}) -> Account_id.

-spec restaurant_id(configuration())->restaurant_id().
restaurant_id(#{restaurant_id:=Restaurant_id}) -> Restaurant_id.

-spec type(configuration())->type().
type(#{type:=Type}) -> Type.

-spec group(configuration())->group().
group(#{group:=Group}) -> Group.

-spec key(configuration())->key().
key(#{key:=Key}) -> Key.

-spec value(configuration())->value().
value(#{value:=Value}) -> Value.

-spec content_type(configuration())->content_type().
content_type(#{content_type:=Content_type}) -> Content_type.    