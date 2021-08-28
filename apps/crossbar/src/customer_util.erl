-module(customer_util).
-include("crossbar.hrl").

-export([
		is_email_exist/1
		,is_phonenumber_exist/1
		,get_sub_fields_customers/1
		,get_all_customers/0
		,get_customer/1
		,get_id/1
		,update_customer_info/2
		,get_customer_by_phonenumber/1
		]).

-export([get_referral_code/0]).


get_customer_by_phonenumber(PhoneNumber) ->
	case catch customer_db:find_by_phone_number(PhoneNumber) of
		[MapVal | _] when is_map(MapVal) ->
			MapVal;
		_ ->
			false
	end.

is_email_exist(Email) ->
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

get_sub_fields_customers(Customer) ->
	Fields = [
		password, 
		created_by, created_time_dt, updated_by, updated_time_dt,
				  confirm_code, confirm_code_created_time_dt, status] ,
	NewMap = maps:without(Fields, Customer),
	Res = maps:to_list(NewMap),
	proplists:substitute_aliases([{id, customer_id},{addresses_arr, addresses},{delivery_addresses_arr, delivery_addresses}], Res).

get_all_customers() ->
	customer_db:find_all().

get_customer(Id) ->
	case catch customer_db:find(Id) of
		MapVal when is_map(MapVal) ->
			MapVal;
		_ ->
			#{}
	end.

-spec update_customer_info(list(),customer_doc:customer_info()) -> any().
update_customer_info(PropUpdateList, Account) ->
	NewAccount = 
		lists:foldl(fun({K, V}, Acc) ->
			if  V == <<>>  ->
				Acc;
			true ->
				maps:put(K, V, Acc)
			end
		end, Account, PropUpdateList),
	customer_db:save(NewAccount).

get_id(CustomerInfo) ->
	customer_doc:id(CustomerInfo).

get_referral_code() ->
    Code = zt_util:to_upper(zt_util:get_uuid(3)),
    case customer_db:find_by_referral_code(Code) of
        [] -> Code;
        _ -> get_referral_code()

    end.

