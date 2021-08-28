-module(access_token_doc).

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-export([token/1, context/1, account_id/1]).

-export([
            create_schema/0,
            delete_schema/0
        ]).

-type scope()    :: list(binary()) | binary().

-opaque context_type() ::
#{
    client => term(),
    resource_owner => term(),
    expiry_time_i => binary(),
    scope => scope()
 }.

-opaque access_token_info() ::
  #{
    token	=> binary(),
    refresh_token	=> binary(),
    account_id => binary(),
    user_id => binary(),
    role => binary(),
    roles => [binary()],
	  context	=> context_type()
   }.

-type token() :: binary().
-type context() :: context_type().
-type account_id() :: binary().

-export_type([access_token_info/0, context/0, context_type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> access_token_info().
sumo_wakeup(Doc) ->
	#{
    	token	=> maps:get(token_id, Doc, <<>>),
    	refresh_token	=> maps:get(refresh_token_id, Doc, <<>>),
      account_id => maps:get(account_id, Doc, <<>>),
      user_id => maps:get(user_id, Doc, <<>>),
    	context	=> maps:get(context, Doc, #{}),
      role => maps:get(role_id, Doc, <<>>),
      roles => maps:get(roles_arr, Doc, [])
     }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(access_token_info()) -> sumo:doc().
sumo_sleep(Info) ->
  #{
      token_id => maps:get(token, Info, <<>>),
      refresh_token_id => maps:get(refresh_token, Info, <<>>),
      account_id => maps:get(account_id, Info, <<>>),
      user_id => maps:get(user_id, Info, <<>>),
      context => maps:get(context, Info, #{}),
      role_id => maps:get(role, Info, <<>>),
      roles_arr => maps:get(roles, Info, [])
     }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
  	sumo:new_field(token_id, binary, [not_null, id]),
  	sumo:new_field(refresh_token_id, binary, [not_null, id]),
    sumo:new_field(account_id, binary, [not_null]),
    sumo:new_field(user_id, binary, [not_null]),
    sumo:new_field(context, object, [not_null]),
    sumo:new_field(role_id, binary),
    sumo:new_field(roles_arr, string)
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

%% @doc Returns token  of the access token.
-spec token(access_token_info()) -> token().
token(#{token := Token})  ->
   Token.

-spec account_id(access_token_info()) -> account_id().
account_id(#{account_id := AccountId}) ->
  AccountId.


-spec context(access_token_info()) -> context().
context(#{context := Context}) ->
  Context.
