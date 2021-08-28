-module(refresh_token_db).

%%% Refresh Token API.
-export([save/1, 
		  del/1, del_by_token/1, del_by_refresh_token/1, 
		  del_all/0,
      find/1, find_all/0, 
		  find_all/2,reindex/0 ]).

-spec save(refresh_token_doc:refresh_token()) -> ok.
save(RefreshToken) ->
  sumo:persist(refresh_token_doc, RefreshToken).

-spec del(refresh_token_doc:refresh_token()) -> boolean().
del(RefreshToken) ->
  sumo:delete(refresh_token_doc, refresh_token_doc:token(RefreshToken)).

-spec del_by_refresh_token(binary()) -> boolean().
del_by_refresh_token(Token) ->
  sumo:delete(refresh_token_doc, Token).

-spec del_by_token(binary()) -> boolean().
del_by_token(Token) ->
  sumo:delete_by(refresh_token_doc, [{access_token_id, Token}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(refresh_token_doc).

-spec find(binary()) -> refresh_token_doc:refresh_token()|notfound.
find(Token) ->
  sumo:find(refresh_token_doc, Token).

-spec find_all() -> [refresh_token_doc:refresh_token()].
find_all() ->
  sumo:find_all(refresh_token_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [refresh_token_doc:refresh_token()].
find_all(Limit, Offset) ->
  sumo:find_all(refresh_token_doc, [], Limit, Offset).


reindex()->
  Docs = find_all(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  save(H),
  reindex(T).
