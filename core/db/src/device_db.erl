-module(device_db).

%%% Device API
-export([
          save/1,
    		  del/1, 
          del_by_id/1,
          del_all/0, 
          find/1, 
          find_by_client_id/1,
          find_by_account_id/1, 
          find_active_by_account_id/1,
          find_active_by_account_id/2,
          find_active_by_user_id/1,
          find_active_by_customer_id/1,
          del_by_account_id/1,
          find_all/0, 
          find_all/2,
          find_by_name/3, 
          find_to_push/1,
          find_by_conditions/4, 
          reindex/0,
    			find_by_account_id_and_os_type/2
      ]).
-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

-define(DOC,device_doc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Device API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new device.

-spec save(?DOC:info()) -> ok.
save(Device) ->
  sumo:persist(device_doc, Device).

%% @doc Deletes the given device.
-spec del(?DOC:info()) -> boolean().
del(Device) ->
  sumo:delete(device_doc, device_doc:id(Device)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(device_doc, Id).

-spec del_by_account_id(binary()) -> boolean().
del_by_account_id(AccountId) ->
  sumo:delete_by(device_doc, [{account_id, AccountId}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(device_doc).

-spec find(binary()) -> ?DOC:info()|notfound.
find(Id) ->
  sumo:find(device_doc, Id).

-spec find_by_account_id(binary()) -> [?DOC:info()].
find_by_account_id(AccountId) ->
  sumo:find_by(device_doc, [{account_id, AccountId}]).

-spec find_by_client_id(binary()) -> [?DOC:info()].
find_by_client_id(ClientId) ->
  sumo:find_by(device_doc, [{client_id, ClientId}]).

-spec find_active_by_account_id(binary()) -> [?DOC:info()].
find_active_by_account_id(AccountId) ->
  sumo:find_by(device_doc, [
    {status, <<"active">>},
    {account_id, AccountId}, 
    {push_id, '/=', <<>>}
  ]).

find_active_by_account_id(AccountId, []) ->
  find_active_by_account_id(AccountId);

find_active_by_account_id(AccountId, OsTypes) ->
  sumo:find_by(device_doc, [
        {status, <<"active">>},
        {account_id, AccountId},
        {push_id, '/=', <<>>},
        {os_type,'in', OsTypes}
      ]).

-spec find_by_account_id_and_os_type(binary(), binary()) -> [?DOC:info()].
find_by_account_id_and_os_type(AccountId, OsType) ->
  sumo:find_by(device_doc, [
    {account_id, AccountId}, 
    {os_type, OsType}
  ]).

-spec find_active_by_user_id(binary()) -> [?DOC:info()].
find_active_by_user_id(UserId) ->
  sumo:find_by(device_doc, [
    {status, <<"active">>},
    {account_scope, <<"USER">>},
    {client_id, UserId}, 
    {push_id, '/=', <<>>}
  ]).

-spec find_active_by_customer_id(binary()) -> [?DOC:info()].
find_active_by_customer_id(CustomerId) ->
  sumo:find_by(device_doc, [
    {status, <<"active">>},
    {account_scope, <<"CUSTOMER">>},
    {client_id, CustomerId}, 
    {push_id, '/=', <<>>}
  ]).

-spec find_all() -> [?DOC:info()].
find_all() ->
  sumo:find_all(device_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_all(Limit, Offset) ->
  sumo:find_all(device_doc, [], Limit, Offset).

-spec find_by_name(binary(), non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_by_name(MqttPassword, Limit, Offset) ->
    sumo:find_by(device_doc, [{mqtt_password_id, MqttPassword}], Limit, Offset).

%-spec find_by_conditions(conditions(),conditions(), non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
%find_by_conditions(OsType, SortCreateTime , Limit, Offset) ->
%    sumo:find_by(device_doc,[{os_type, OsType}]  ,[{created_time, SortCreateTime}] , Limit, Offset).


-spec find_to_push(binary()) -> [?DOC:info()].
find_to_push(AccountQuery) ->
	sumo:find_by(device_doc, [
    {push_id, '/=', <<>>}, 
    {status, <<"active">>}|AccountQuery], [{created_time, asc}], 20, 0).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(device_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->

  Condition = if

        Key == <<"sort_created_time">> -> {created_time, Value};
        Key == <<"sort_updated_time">> -> {updated_time, Value};
        true -> ignore
      end,
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  build_sort(NewQuery, Tail);

build_sort(Query, _Other) when is_list(Query) -> Query;

build_sort(_Query, _Other) ->[].

build_query(Query, [{Key, Value}| Tail]) when is_list(Query) ->
  Condition = if

        Key == <<"filter_created_by">>  -> {created_by, Value};
        Key == <<"filter_created_time_gt">>  -> {created_time, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_gte">>  -> {created_time, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lt">>  -> {created_time, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lte">>  -> {created_time, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time">>  -> {created_time, datetime_util:utc_format(Value)};
        Key == <<"filter_updated_by">>  -> {updated_by, Value};
        Key == <<"filter_updated_time_gt">>  -> {updated_time, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_gte">>  -> {updated_time, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lt">>  -> {updated_time, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lte">>  -> {updated_time, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time">>  -> {updated_time, datetime_util:utc_format(Value)};
        true -> ignore
      end,
  NewQuery =
  case Condition of
    ignore -> Query;
    _ ->
      [Condition|Query]
  end,
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) -> Query;

build_query(_Query, _Other) -> [].

reindex()->
  Docs = find_all(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;

reindex([H|T]) ->
  save(H),
  reindex(T).
