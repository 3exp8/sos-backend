-module(access_token_mnesia_doc).

-include("db.hrl").

-export([sleep/1, wakeup/1]).

-type scope()    :: list(binary()) | binary().

-opaque context_type_info() ::
#{
    client => term(),
    resource_owner => term(),
    expiry_time => binary(),
    scope => scope()
 }.

-opaque access_token_info() ::
  #{
    token	=> binary(),
    refresh_token => binary(),
    user_id => binary(),
    account_id => binary(),
    role => binary(),
    roles => [map()],
	  context	=> context_type_info()
   }.
 -type context() :: context_type_info().

-export_type([access_token_info/0, context/0, context_type_info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.

-spec wakeup(#access_token_mnesia_doc{}) -> access_token_info().
wakeup(Doc) ->
#{
    token	=>  Doc#access_token_mnesia_doc.token,
    refresh_token =>  Doc#access_token_mnesia_doc.refresh_token,
    user_id => Doc#access_token_mnesia_doc.user_id,
    account_id => Doc#access_token_mnesia_doc.account_id,
    role => Doc#access_token_mnesia_doc.role,
    roles => Doc#access_token_mnesia_doc.roles,
    context	=> Doc#access_token_mnesia_doc.context
    
}.

%% @doc Part of the sumo_doc behavior.
-spec sleep(access_token_info()) -> #access_token_mnesia_doc{}.
sleep(AccessTokenInfo) ->
   #access_token_mnesia_doc{
      token = maps:get(token, AccessTokenInfo, <<>>),
      refresh_token = maps:get(refresh_token, AccessTokenInfo, <<>>),
      user_id = maps:get(user_id, AccessTokenInfo, <<>>),
      account_id = maps:get(account_id, AccessTokenInfo, <<>>),
      role = maps:get(role, AccessTokenInfo, <<>>),
      roles = maps:get(roles, AccessTokenInfo, []),
      context = maps:get(context, AccessTokenInfo, #{})
    }.