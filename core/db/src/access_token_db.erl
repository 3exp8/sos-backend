-module(access_token_db).

%%% Access Token API.
-export([save/1, 
		  del/1, del_by_token/1, del_by_refresh_token/1,
      del_all/0, del_by_account_id/1,
      find_by_token/1, find_all/0, 
		  find_by_account_id/1, find_by_account_id/3,
      find_all/2,reindex/0 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access Token API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec save(access_token_doc:access_token_info()) -> ok.
save(Info) ->
  sumo:persist(access_token_doc, Info).

-spec del(access_token_doc:access_token_info()) -> boolean().
del(AccessToken) ->
  sumo:delete(access_token_doc, access_token_doc:token(AccessToken)).

-spec del_by_token(binary()) -> boolean().
del_by_token(Token) ->
  sumo:delete(access_token_doc, Token).

-spec del_by_account_id(binary()) -> boolean().
del_by_account_id(AccountId) ->
  sumo:delete_by(access_token_doc, [{account_id, AccountId}]).

-spec del_by_refresh_token(binary()) -> boolean().
del_by_refresh_token(Token) ->
  sumo:delete_by(refresh_token_doc, [{refresh_token_id, Token}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(access_token_doc).

-spec find_by_token(binary()) -> access_token_doc:access_token_info()|notfound.
find_by_token(Token) ->
  sumo:find(access_token_doc, Token).

-spec find_by_account_id(binary()) -> access_token_doc:access_token_info()|notfound.
find_by_account_id(AccountId) ->
  sumo:find_by(access_token_doc, [{account_id, AccountId}]).

-spec find_by_account_id(binary(), non_neg_integer(), non_neg_integer()) -> 
                                                          access_token_doc:access_token_info()|notfound.
find_by_account_id(AccountId, Limit, Offset) ->
  sumo:find_by(access_token_doc, [{account_id, AccountId}], Limit, Offset).

-spec find_all() -> [access_token_doc:access_token_info()].
find_all() ->
  sumo:find_all(access_token_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [access_token_doc:access_token_info()].
find_all(Limit, Offset) ->
  sumo:find_all(access_token_doc, [], Limit, Offset).

reindex()->
  Docs = find_all(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  save(H),
  reindex(T).
