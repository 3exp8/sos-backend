-module(access_token_mnesia_db).

-include("db.hrl").

-export([
      create_table/0,
      delete_table/0,
      save/1,
      async_save/1,
      find/1, 
      find_by_token/1,
      find_by_account_id/1,
      find_by_user_id/1,
      find_all/0,
      match/1,
      async_delete/1,
      delete/1,
      del_all/0,
      del_by_account_id/1,
      del_by_token/1,
      add_indexs/0
    ]).

-export([table_id/0]).

table_id() -> access_token_mnesia_doc.

create_table() ->
      mnesia:create_table(access_token_mnesia_doc, [
                    {record_name, access_token_mnesia_doc},
                    {disc_copies, [node()]},
                    {attributes, record_info(fields, access_token_mnesia_doc)}
  ]),
  add_indexs().
  

add_indexs() ->

 mnesia:add_table_index(access_token_mnesia_doc, user_id),
   mnesia:add_table_index(access_token_mnesia_doc, account_id),
   mnesia:add_table_index(access_token_mnesia_doc, token),
  mnesia:add_table_index(access_token_mnesia_doc, refresh_token).

delete_table() ->
  mnesia:delete_table(access_token_mnesia_doc).

save(Info) ->
  Doc = access_token_mnesia_doc:sleep(Info),
  Fun = fun() ->
    mnesia:write(Doc)
  end,
  mnesia:sync_dirty(Fun).

async_save(Info) ->
  Doc = access_token_mnesia_doc:sleep(Info),
  mnesia:dirty_write(Doc).

find(Token) ->
  case mnesia:dirty_read(access_token_mnesia_doc, Token) of 
  [#access_token_mnesia_doc{} = Doc] ->

    access_token_mnesia_doc:wakeup(Doc);
  _ ->
    notfound
  end.

find_by_account_id(AccountId) ->
  Pattern = #access_token_mnesia_doc{account_id = AccountId,_ = '_'},
  Docs = mnesia:dirty_match_object(Pattern),
  lists:filtermap(fun(Doc)-> 
    {true, access_token_mnesia_doc:wakeup(Doc)}
  end,Docs).

find_by_user_id(UserId) ->
  Pattern = #access_token_mnesia_doc{user_id = UserId,_ = '_'},
  Docs = mnesia:dirty_match_object(Pattern),
  lists:filtermap(fun(Doc)-> 
    {true, access_token_mnesia_doc:wakeup(Doc)}
  end,Docs).

find_by_token(Token) ->
  Pattern = #access_token_mnesia_doc{token = Token,_ = '_'},
  Docs = mnesia:dirty_match_object(Pattern),
  Tokens =
  lists:filtermap(fun(Doc)-> 
    {true, access_token_mnesia_doc:wakeup(Doc)}
  end,Docs),
  case Tokens of 
    [TokeInfo] -> TokeInfo;
    _ -> notfound
  end.

find_all() ->
  Records = ets:tab2list(access_token_mnesia_doc),
  lists:map(fun(Rec) -> access_token_mnesia_doc:wakeup(Rec) end, Records).


match(Pattern) ->
  case  mnesia:dirty_select(access_token_mnesia_doc, Pattern) of 
  [#access_token_mnesia_doc{} | _] = StatusList -> StatusList;
  _ -> []
  end. 

async_delete(Key) ->
  mnesia:dirty_delete(access_token_mnesia_doc, Key).

delete(Key) ->
  mnesia:dirty_delete(access_token_mnesia_doc, Key).

del_all() ->
  %mnesia:dirty_delete(access_token_mnesia_doc, Key).
  todo.

del_by_account_id(AccountId) ->
  TokenList = find_by_account_id(AccountId),
  lists:foreach(fun(#{token := Token}) -> 
    delete(Token)
  end,TokenList).

del_by_token(Token) ->
  delete(Token).
% del_by_token(Token) ->
%   TokenList = find_by_token(Token),
%   lists:foreach(fun(#{token := Token}) -> 
%     delete(Token)
%   end,TokenList).  