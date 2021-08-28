-module(role_doc).

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([id/1, name/1, permissions/1, description/1]).

-export([
            create_schema/0,
            delete_schema/0,
            init_data/0
        ]).

-opaque role_info() ::
#{
    id => binary(),
    account_id => binary(),
    name => binary(),
    permissions => [binary()],
    description => binary(),
    created_by =>  binary(),
    created_time =>  binary(),
    updated_by =>  binary(),
    updated_time => binary()
 }.

-type id() :: binary().
-type name() :: binary().
-type permissions() :: [binary()].
-type description() :: binary().

-export_type([role_info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> role_info().
sumo_wakeup(Doc) ->
	#{
    	id => maps:get(id, Doc, <<>>),
      account_id => maps:get(account_id, Doc, <<>>),
      name   => maps:get(name, Doc, <<>>),
    	permissions   => maps:get(permissions_arr, Doc, <<>>),
      description => maps:get(description, Doc, <<>>),
      created_by => maps:get(created_by_id, Doc, <<>>),
      created_time => maps:get(created_time_dt, Doc, <<>>),
      updated_by => maps:get(updated_by_id, Doc, <<>>),
      updated_time => maps:get(updated_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(role_info()) -> sumo:doc().
sumo_sleep(PermissionInfo) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
      id => maps:get(id, PermissionInfo, <<>>),
      account_id => maps:get(account_id, PermissionInfo, <<>>),
      name   => maps:get(name, PermissionInfo, <<>>),
      permissions_arr   => maps:get(permissions, PermissionInfo, <<>>),
      description => maps:get(description, PermissionInfo, <<>>),
      created_by_id => maps:get(created_by, PermissionInfo, <<>>),
      created_time_dt => maps:get(created_time, PermissionInfo, <<>>),
      updated_by_id => maps:get(updated_by, PermissionInfo, <<>>),
      updated_time_dt => maps:get(updated_time, PermissionInfo, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
  	sumo:new_field(id, binary, [not_null, id]),
    sumo:new_field(account_id, binary),
    sumo:new_field(name, binary),
  	sumo:new_field(permissions_arr, string),
    sumo:new_field(description, string),
    sumo:new_field(created_by_id , binary),
    sumo:new_field(created_time_dt , datetime),
    sumo:new_field(updated_by_id , binary),
    sumo:new_field(updated_time_dt , datetime)
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
  
-spec id(role_info()) ->id().
    id(#{id:= Id}) -> Id.

-spec name(role_info()) -> name().
name(#{name := Name}) ->
    Name.

-spec permissions(role_info()) -> permissions().
permissions(#{permissions := Permissions}) ->
  Permissions.

-spec description(role_info()) -> description().
description(#{description := Description}) ->
  Description.

init_data() ->
 R = 
 #{account_id => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   created_by => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   created_time => <<"2019-07-19T08:09:19Z">>,
   description => <<"admin">>,
   id => <<"7feb9695746a66da6725046c34fc7a73">>,
   name => <<"admin">>,
   permissions => [<<"all">>],
   updated_by => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   updated_time => <<"2019-08-07T16:17:20Z">>},
   role_db:save(R).