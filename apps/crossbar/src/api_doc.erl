-module(api_doc).
-include("crossbar.hrl").

-export([is_user_exist/1
		,is_phonenumber_exist/1
		,get_account_id_by_token/1
]).

-export([del_tokens_of_user/1]).


is_user_exist(Email) ->
	case catch customer_db:find_by_email(Email) of
		[Val| _]  when is_map(Val) ->
			true;
		_ ->
			false
	end. 

is_phonenumber_exist(PhoneNumber) ->
	case catch customer_db:find_by_phone_number(PhoneNumber) of
		[Val| _]  when is_map(Val) ->
			true;
		_ ->
			false
	end. 
	

del_tokens_of_user(AccountId) ->
	access_token_mnesia_db:del_by_account_id(AccountId).

get_account_id_by_token(Token) ->
	TokenDoc = access_token_mnesia_db:find_by_token(Token),
	case is_map(TokenDoc) of
		false ->
				<<>>;
		_ ->
			maps:get(account_id,TokenDoc)
	end.