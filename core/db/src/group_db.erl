-module(group_db).

%%% User API
-export([
            save/1,
            del/1,
            del_by_id/1,
            del_all/0,
            find/1,
            find_all/0,
            find_all/2,
            find_by_conditions/4,
            find_count_by_conditions/4,
            reindex/0,
            reindex/1
        ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].
-define(DOC, group_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% User API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new account.

-spec save(?DOC:info()) -> ok.
save(Info) ->
  sumo:persist(?DOC, Info).

%% @doc Deletes the given account.
-spec del(?DOC:info()) -> boolean().
del(#{id := Id}) ->
  sumo:delete(?DOC, Id).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(?DOC, Id).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(?DOC).

-spec find(binary()) -> ?DOC:info()|notfound.
find(Id) ->
  sumo:find(?DOC, Id).

-spec find_all() -> [?DOC:info()].
find_all() ->
  sumo:find_all(?DOC).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_all(Limit, Offset) ->
  sumo:find_all(?DOC, [], Limit, Offset).


-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  sumo:find_by(?DOC, Conditions, SortOrders, Limit, Offset).

-spec find_count_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [?DOC:info()].
find_count_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  Vals = sumo:find_by(?DOC, Conditions, SortOrders, Limit, Offset),
  {length(Vals),Vals}.

build_sort([], []) ->
  [{type, asc},{name, asc}];

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  Condition = if  
        Key == <<"sort_created_by">> -> {created_by, Value};
        Key == <<"sort_created_time">> -> {created_time, Value};
        Key == <<"sort_updated_by">> -> {updated_by, Value};
        Key == <<"sort_updated_time">> -> {updated_time, Value};
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
        Key == <<"filter_verify_status">>  -> {verify_status, Value};
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
    _ -> [Condition|Query]
  end,
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query ;

build_query(_Query, _Other) ->
  [].

reindex()->
  Docs = fetch_all([], 5, 0),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  NewInfo = maps:merge(H, #{
      account_id => maps:get(id, H, <<>>),
      roles => [#{
        roles => [<<"admin">>],
        related_type => <<"all">>,
        related_id => <<"all">>
      }]
    }),
  save(NewInfo),
  reindex(T).

fetch_all(CurRes, Limit, Offset) ->
  lager:debug("fetch_all limit: ~p, Offset: ~p~n",[Limit, Offset]),
  case find_all(Limit, Offset) of 
    [] -> CurRes;
    NewRes -> fetch_all(NewRes ++ CurRes, Limit, Offset + Limit)
  end.