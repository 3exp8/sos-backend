-module(role_db).

%%% Roleser API
-export([save/1,
		  find_all/0, 
      find/1, 
      find_by_name/1,
      find_by_permission/1,
      find_by_account_id/1,
      find_by_conditions/4, 
      del_by_id/1, 
      del_all/0, 
      reindex/0 ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% role API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Updates an role.
-spec save(role_doc:role_info()) -> ok.
save(Role) ->
  sumo:persist(role_doc, Role).

-spec del(role_doc:role_info()) -> boolean().
del(RoleInfo) ->
  sumo:delete(role_doc, role_doc:id(RoleInfo)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(role_doc, Id).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(role_doc).

-spec find(binary()) -> role_doc:role_info()|notfound.
find(Id) ->
  sumo:find(role_doc, Id).

-spec find_all() -> [role_doc:role_info()].
find_all() ->
  sumo:find_all(role_doc).

-spec find_by_name(binary()) -> [role_doc:role_info()].
find_by_name(Name) ->
  sumo:find_by(role_doc,[{name, Name}]).

-spec find_by_account_id(binary()) -> [role_doc:role_info()].
find_by_account_id(AccountId) ->
  sumo:find_by(role_doc,[{account_id, AccountId}]).

-spec find_by_permission(binary()) -> [role_doc:role_info()].
find_by_permission(Permission) ->
  sumo:find_by(role_doc,[{permissions_arr, Permission}]).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [role_doc:role_info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(role_doc, Conditions, SortOrders, Limit, Offset).

build_sort([], []) ->
  [{name, asc}];

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->

  Condition = if
        Key == <<"sort_name">>  -> {name, Value};
        Key == <<"sort_created_by">> -> {created_by_id, Value};
        Key == <<"sort_created_time">> -> {created_time_dt, Value};
        Key == <<"sort_updated_by">> -> {updated_by_id, Value};
				Key == <<"sort_updated_time">> -> {updated_time_dt, Value};
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
      
      Key == <<"filter_name">>  -> {name, Value};
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
  NewH = maps:merge(H, #{
      account_id => <<"accountddfc2382b892fae1d4857b8cae670e73">>
    }),
  save(NewH),
  reindex(T).
