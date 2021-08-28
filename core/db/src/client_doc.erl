-module(client_doc).

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/1, client_id/1,secret_key/1, redirect_uri/1]).

-export([
            create_schema/0,
            delete_schema/0
        ]).

-opaque client() ::
  #{
    client_id   => binary(),
    secret_key   => binary(),
    account_id => binary(),
    account_role => binary(),
    client_name => binary(),
    email => binary(),
    logo => binary(),
    url_info => binary(),
    description => binary(),
    redirect_uri => binary(),
    created_by => binary(),
    created_time_dt => binary(),
    updated_by => binary(),
    updated_time_dt => binary()

  }.

-type client_id()	 :: binary().
-type secret_key() 	 :: binary().
-type redirect_uri() :: binary().

-export_type([client/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> client().
sumo_wakeup(Doc) ->
	#{
    client_id  => maps:get(client_id, Doc, <<>>),
    secret_key   => maps:get(secret_key, Doc, <<>>),
    account_id => maps:get(account_id, Doc, <<>>),
    account_role => maps:get(account_role, Doc, <<>>),
    client_name => maps:get(client_name, Doc, <<>>),
    email => maps:get(email, Doc, <<>>),
    logo => maps:get(logo, Doc, <<>>),
    url_info => maps:get(url_info, Doc, <<>>),
    description => maps:get(description, Doc, <<>>),
    redirect_uri => maps:get(redirect_uri, Doc, <<>>),
    created_by => maps:get(created_by, Doc, <<>>),
    created_time_dt => maps:get(created_time_dt, Doc, <<>>),
    updated_by => maps:get(updated_by, Doc, <<>>),
    updated_time_dt => maps:get(updated_time_dt, Doc, <<>>)
    
     }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(client()) -> sumo:doc().
sumo_sleep(Client) ->
    lager:info("Client: ~p ~n",[Client]),
   DefaultTime = util_db:now_to_utc_binary({0,0,0}),
  #{
    client_id   => maps:get(client_id, Client, <<>>),
    secret_key   => maps:get(secret_key, Client, <<>>),
    account_id => maps:get(account_id, Client, <<>>),
    account_role => maps:get(account_role, Client, <<>>),
    client_name => maps:get(client_name, Client, <<>>),
    email => maps:get(email, Client, <<>>),
    logo => maps:get(logo, Client, <<>>),
    url_info => maps:get(url_info, Client, <<>>),
    description => maps:get(description, Client, <<>>),
    redirect_uri => maps:get(redirect_uri, Client, <<>>),
    created_by => maps:get(created_by, Client, <<>>),
    created_time_dt => maps:get(created_time_dt, Client, DefaultTime),
    updated_by => maps:get(updated_by, Client, <<>>),
    updated_time_dt => maps:get(updated_time_dt, Client, DefaultTime)
     }.
     
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(client_id, binary, [not_null, id]),
    sumo:new_field(secret_key, binary, [ not_null]),
    sumo:new_field(account_id, binary, [not_null]),
    sumo:new_field(account_role, binary, [not_null]),
    sumo:new_field(client_name, string, [ not_null]),
    sumo:new_field(email, string, [ not_null]),
    sumo:new_field(logo, string),
    sumo:new_field(url_info, string),
    sumo:new_field(description, text),
    sumo:new_field(redirect_uri, string),
    sumo:new_field(created_by, binary),
    sumo:new_field(created_time_dt, datetime),
    sumo:new_field(updated_by, binary),
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
  

new({ClientId, SecretKey, AccountId, AccountRole, ClientName, Email, Logo, UrlInfo, Description, RedirectUri,
    CreatedBy, CreatedTime, UpdatedBy, UpdatedTime}) ->
  #{
    client_id   => ClientId,
    secret_key   => SecretKey,
    account_id => AccountId,
    account_role => AccountRole,
    client_name => ClientName,
    email => Email,
    logo => Logo,
    url_info => UrlInfo,
    description => Description,
    redirect_uri => RedirectUri,
    created_by => CreatedBy,
    created_time_dt => CreatedTime,
    updated_by => UpdatedBy,
    updated_time_dt => UpdatedTime
  }. 


%% @doc Returns client_id of client.
-spec client_id(client()) -> client_id().
client_id(#{client_id := ClientId})  ->
   ClientId.

%% @doc Returns secret key of client.
-spec secret_key(client()) -> secret_key().
secret_key(#{secret_key := SecretKey}) ->
  SecretKey.

%% @doc Returns  redirect_uri of client.
-spec redirect_uri(client()) -> redirect_uri().
redirect_uri(#{redirect_uri := RedirectUri}) ->
  RedirectUri.