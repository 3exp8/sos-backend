-module(permission_db).

%%% Permissionser API
-export([save/1,
		  find_all/0, find_by_id/1, find_by_conditions/4, del_by_id/1, reindex/0 ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% permission API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Updates an permission.
-spec save(permission_doc:permission_info()) -> ok.
save(Permission) ->
  sumo:persist(permission_doc, Permission).

-spec del(permission_doc:permission_info()) -> boolean().
del(PermissionInfo) ->
  sumo:delete(permission_doc, permission_doc:id(PermissionInfo)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(permission_doc, Id).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(permission_doc).

-spec find_by_id(binary()) -> permission_doc:permission_info()|notfound.
find_by_id(Id) ->
  sumo:find(permission_doc, Id).

-spec find_all() -> [permission_doc:permission_info()].
find_all() ->
  sumo:find_all(permission_doc).


-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [permission_doc:permission_info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(permission_doc, Conditions, SortOrders, Limit, Offset).

build_sort([], []) ->
  [{created_time_dt, desc}];
  
build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->
  Condition = if
        Key == <<"name">>  -> {name_id, Value};
        Key == <<"sort_type">> -> {type_id, Value};
        Key == <<"sort_created_time">> -> {created_time_dt, Value};
				Key == <<"sort_update_time">> -> {update_time_dt, Value};
				Key == <<"description">> -> {description_id, Value};
				Key == <<"updated_by">> -> {updated_by_id, Value};
				Key == <<"created_by">> -> {created_by_id, Value};
        true -> ignore
      end,
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  build_sort(NewQuery, Tail) ;

build_sort(Query, _Other) when is_list(Query) ->
  Query;

build_sort(_Query, _Other) ->
  build_sort([], []).

build_query(Query, [{Key, Value}| Tail]) when is_list(Query) ->
  Condition = if
      Key == <<"name">>  -> {name_id, Value};
			Key == <<"type">>  -> {type_id, Value};
			Key == <<"description">>  -> {description_id, Value};
			Key == <<"created_by">>  -> {created_by_id, Value};
			Key == <<"updated_by">>  -> {updated_by_id, Value};
			Key == <<"filter_created_time_gt">>  -> {created_time_dt, '>', datetime_util:utc_format(Value)};
			Key == <<"filter_created_time_gte">>  -> {created_time_dt, '>=', datetime_util:utc_format(Value)};
			Key == <<"filter_created_time_lt">>  -> {created_time_dt, '<', datetime_util:utc_format(Value)};
			Key == <<"filter_created_time_lte">>  -> {created_time_dt, '=<', datetime_util:utc_format(Value)};
			Key == <<"filter_created_time">>  -> {created_time_dt, datetime_util:utc_format(Value)};
			Key == <<"filter_updated_time_gt">>  -> {updated_time_dt, '>', datetime_util:utc_format(Value)};
			Key == <<"filter_updated_time_gte">>  -> {updated_time_dt, '>=', datetime_util:utc_format(Value)};
			Key == <<"filter_updated_time_lt">>  -> {updated_time_dt, '<', datetime_util:utc_format(Value)};
			Key == <<"filter_updated_time_lte">>  -> {updated_time_dt, '=<', datetime_util:utc_format(Value)};
			Key == <<"filter_updated_time">>  -> {updated_time_dt, datetime_util:utc_format(Value)};
        true -> ignore
      end,
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query;

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
