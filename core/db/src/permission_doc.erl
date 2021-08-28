-module(permission_doc).

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([id/1, name/1, type/1, description/1]).

-export([
            create_schema/0,
            delete_schema/0
        ]).

-opaque permission_info() ::
#{
    id => binary(),
    name => binary(),
    type => binary(),
    description => binary(),
    created_time =>  binary(),
    updated_time => binary()
 }.

-type id() :: binary().
-type name() :: binary().
-type type() :: binary().
-type description() :: binary().

-export_type([permission_info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> permission_info().
sumo_wakeup(Doc) ->
	#{
    	name   => maps:get(name, Doc, <<>>),
    	type   => maps:get(type, Doc, <<>>),
      id => maps:get(id, Doc, <<>>),
      description => maps:get(description, Doc, <<>>),
      created_time => maps:get(created_time_dt, Doc, <<>>),
      updated_time => maps:get(updated_time_dt, Doc, <<>>),
      updated_by => maps:get(updated_by, Doc, <<>>),
      created_by => maps:get(created_by, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(permission_info()) -> sumo:doc().
sumo_sleep(PermissionInfo) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
      name => maps:get(name, PermissionInfo, <<>>),
      type_id => maps:get(type, PermissionInfo, <<>>),
      id => maps:get(id, PermissionInfo, <<>>),
      description_id => maps:get(description, PermissionInfo, <<>>),
      created_time_dt => maps:get(created_time, PermissionInfo, DefaultTime),
      updated_time_dt => maps:get(updated_time, PermissionInfo, DefaultTime),
      created_by_id => maps:get(created_by, PermissionInfo, <<>>),
      updated_by_id => maps:get(updated_by, PermissionInfo, <<>>)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id, binary, [not_null, id]),
  	sumo:new_field(name, string),
  	sumo:new_field(type_id, binary),
    sumo:new_field(description_id, string),
    sumo:new_field(created_by_id, binary),
    sumo:new_field(created_time_dt, datetime),
    sumo:new_field(updated_by_id, binary),
    sumo:new_field(updated_time_dt, datetime)
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

-spec id(permission_info()) ->id().
    id(#{id:= Id}) -> Id.

-spec name(permission_info()) -> name().
name(#{name := Name}) ->
    Name.

-spec type(permission_info()) -> type().
type(#{type := Type}) ->
  Type.

-spec description(permission_info()) -> description().
description(#{description := Description}) ->
  Description.
