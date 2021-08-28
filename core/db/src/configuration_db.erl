-module(configuration_db).

-export([save/1,del/1,del_by_id/1,
        find/1,find_all/2,find_all/0,
        find_by_group/1,find_by_key/1,find_by_type/1,
        find_by_restaurant_id/1,
        find_by_conditions/4]).

%% DEBUG

-type condition()::{atom(),atom()}.
-type conditions()::[condition()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Vehicle API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Updates an vehicle.
-spec save(configuration_doc:configuration()) -> ok.
save(Configuration) ->
  lager:debug("~p~n",[Configuration]),
  sumo:persist(configuration_doc, Configuration).

%% @doc Deletes the given customer.
-spec del(configuration_doc:configuration()) -> boolean().
del(Configuration) ->
  sumo:delete(configuration_doc, configuration_doc:id(Configuration)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(configuration_doc, Id).

%% @doc Finds an vehicel, given the ID.
-spec find(binary()) -> configuration_doc:configuration()|notfound.
find(Id) ->
  sumo:find(configuration_doc, Id).

-spec find_by_group(binary()) -> configuration_doc:configuration()|notfound.
find_by_group(Group) ->
  sumo:find_by(configuration_doc, [{group,Group}]).

-spec find_by_key(binary()) -> configuration_doc:configuration()|notfound.
find_by_key(Key) ->
  sumo:find_by(configuration_doc, [{key,Key}]).

-spec find_by_type(binary()) -> configuration_doc:configuration()|notfound.
find_by_type(Type) ->
  sumo:find_by(configuration_doc, [{type,Type}]).

  -spec find_by_restaurant_id(binary()) -> configuration_doc:configuration()|notfound.
find_by_restaurant_id(Restaurant_id) ->
  sumo:find_by(configuration_doc, [{restaurant_id,Restaurant_id}]).

-spec find_all() -> [configuration_doc:configuration()].
find_all() ->
  sumo:find_all(configuration_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [configuration_doc:configuration()].
find_all(Limit, Offset) ->
  sumo:find_all(configuration_doc, [], Limit, Offset).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [configuration_doc:configuration()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  sumo:find_by(configuration_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if  
        Key == <<"sort_order">>  -> {order, Value};
        Key == <<"sort_account_id">>  -> {account_id, Value};
        Key == <<"sort_restaurant_id">>  -> {restaurant_id, Value};
        Key == <<"sort_type">> -> {type,Value};
        Key == <<"sort_group">> -> {group,Value};
        Key == <<"sort_key">> -> {key,Value};
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
        Key == <<"filter_account_id">>  -> {account_id, Value};
        Key == <<"filter_restaurant_id">>  -> {restaurant_id, Value};
        Key == <<"filter_type">>  -> {type, Value};
        Key == <<"filter_group">>  -> {group, Value};
        Key == <<"filter_key">>  -> {key, Value};
        Key == <<"filter_content_type">> -> {content_type,Value};
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

build_query(Query, _Other) when is_list(Query) -> Query;

build_query(_Query, _Other) -> [].