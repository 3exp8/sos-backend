-module(customer_db).

%%% Customer API
-export([
      save/1,
		  del/1, 
      del_by_id/1, 
      del_by_email/1,
      del_by_phone_number/1, 
      del_all/0, 
		  find/1, 
      find_by_email/1, 
      find_by_phone_number/1,
      find_by_referral_code/1,
      find_all/0, 
      find_all/2, 
      find_by_conditions/4, 
      reindex/0
    ]).


%% DEBUG
-export([
    find_customers_by_phone_number_riak_client/2
    ,find_customers_by_id/1
    ,find_customers_by_id/2
  ]).


-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].
-define(DOC,customer_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Customer API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Updates an customer.

-spec save(?DOC:info()) -> ok.
save(Info) ->
  sumo:persist(customer_doc, Info).

%% @doc Deletes the given customer.
-spec del(?DOC:info()) -> boolean().
del(Customer) ->
  sumo:delete(customer_doc, ?DOC:id(Customer)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(customer_doc, Id).

-spec del_by_email(binary()) -> boolean().
del_by_email(Email) ->
  sumo:delete_by(customer_doc, [{email_id, Email}]).

-spec del_by_phone_number(binary()) -> boolean().
del_by_phone_number(PhoneNumber) ->
  sumo:delete_by(customer_doc, [{phone_number_id, PhoneNumber}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(customer_doc).

%% @doc Finds an customer, given the Email.

-spec find(binary()) -> ?DOC:info()|notfound.
find(Id) ->
  sumo:find(customer_doc, Id).

-spec find_by_email(binary()) -> [?DOC:info()].
find_by_email(Email) ->  
  sumo:find_by(customer_doc, [{email_id, Email}]).

-spec find_by_phone_number(binary()) -> [?DOC:info()].
find_by_phone_number(PhoneNumber) ->  
  sumo:find_by(customer_doc, [{phone_number_id, PhoneNumber}]).

-spec find_by_referral_code(binary()) -> [?DOC:info()].
find_by_referral_code(RefCode) ->  
  sumo:find_by(customer_doc, [{referral_code, RefCode}]).

-spec find_all() -> [?DOC:info()].
find_all() ->
  sumo:find_all(customer_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_all(Limit, Offset) ->
  sumo:find_all(customer_doc, [], Limit, Offset).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  sumo:find_by(customer_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if  
        Key == <<"sort_id">> -> {id, Value};
        Key == <<"sort_email">>  -> {email_id, Value};
        Key == <<"sort_first_name">>  -> {first_name, Value};
        Key == <<"sort_last_name">>  -> {last_name, Value};
        Key == <<"sort_created_by">> -> {created_by_id, Value};
        Key == <<"sort_created_time">> -> {created_time_dt, Value};
        Key == <<"sort_updated_by">> -> {updated_by_id, Value};
        Key == <<"sort_updated_time">> -> {updated_time_dt, Value};
        true -> ignore
      end, 
        lager:debug("------ build_sort: Condition ~p ~n",[Condition]),
  NewQuery = 
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  lager:debug("------ build_sort: NewSort ~p ~n",[NewQuery]),
  build_sort(NewQuery, Tail) ;

build_sort(Query, _Other) when is_list(Query) ->
  Query;

build_sort(_Query, _Other) ->
  [].

build_query(Query, [{Key, Value}| Tail]) when is_list(Query) -> 
lager:debug("------ build_query: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if  
        Key == <<"filter_id">> -> {id, Value};
        Key == <<"filter_email">>  -> {email_id, Value};
        Key == <<"filter_phone">>  -> {phone_number_id, Value};
        Key == <<"filter_first_name">>  -> {first_name, Value};
        Key == <<"filter_last_name">>  -> {last_name, Value};
        Key == <<"filter_time_zone">>  -> {time_zone, Value};
        Key == <<"filter_type">>  -> {type_id, Value};
        Key == <<"filter_created_by">>  -> {created_by_id, Value};
        Key == <<"filter_created_time_gt">>  -> {created_time_dt, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_gte">>  -> {created_time_dt, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lt">>  -> {created_time_dt, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lte">>  -> {created_time_dt, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time">>  -> {created_time_dt, datetime_util:utc_format(Value)};
        Key == <<"filter_updated_by">>  -> {updated_by_id, Value};
        Key == <<"filter_updated_time_gt">>  -> {updated_time_dt, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_gte">>  -> {updated_time_dt, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lt">>  -> {updated_time_dt, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lte">>  -> {updated_time_dt, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time">>  -> {updated_time_dt, datetime_util:utc_format(Value)};        
        true -> ignore
      end, 
  lager:debug("------ build_query: Condition ~p ~n",[Condition]),
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  lager:debug("------ build_query: NewQuery ~p ~n",[NewQuery]),
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query ;

build_query(_Query, _Other) ->
  [].

reindex()->
  Docs = find_all(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  save(H),
  reindex(T).