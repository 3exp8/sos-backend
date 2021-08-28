-module(account_doc).
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
        name =>  maps:get(name, Doc, <<>>),
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
        name =>  maps:get(name, Info, <<>>),
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