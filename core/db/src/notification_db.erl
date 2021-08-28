-module(notification_db).

%%%% NOTIFICATION API
-export([
            save/1,
        	del/1, 
            del_by_id/1, 
            del_all/0,
        	find/1, 
            find_by_destinations/2, 
            find_by_destinations/3, 
            find_by_target/2, 
            find_by_read_by/2, 
            find_by_read_by/3,
        	find_all/0, 
            find_all/2,
        	find_by_conditions/4
    ]).

-type condition() :: {atom(), atom()}.
-type conditions() :: [condition()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Notification API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Creates a new notification.
-spec save(notification_doc:notification()) -> ok.
save(Notification) ->
    
	sumo:persist(notification_doc, Notification).

%% @Deletes the given notification.
-spec del(notification_doc:notification()) -> boolean().
del(Notification) ->
	sumo:delete(notification_doc, notification_doc:id(Notification)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
	sumo:delete(notification_doc, Id).

-spec del_all() -> boolean().
del_all() ->
	sumo:delete_all(notification_doc).

%% @doc Finds the given notification.
-spec find(binary()) -> notification_doc:notification() | notfound.
find(Id) ->
	sumo:find(notification_doc, Id).

-spec find_by_destinations(binary(), binary()) -> notification_doc:notification() | notfound.
-spec find_by_destinations(binary(), binary(), binary()) -> notification_doc:notification() | notfound.
find_by_destinations(Type, Id) ->
	sumo:find_by(notification_doc, [{"destinations#type", Type}, {"destinations#id", Id}]).
find_by_destinations(Type, Id, NotiId) ->
	sumo:find_by(notification_doc, [{"destinations#type", Type}, {"destinations#id", Id}, {id, NotiId}]).

-spec find_by_target(binary(), binary()) -> notification_doc:notification() | notfound.
find_by_target(Type, Id) -> 
	sumo:find_by(notification_doc, [{target_type, Type}, {target_id, Id}]).

-spec find_by_read_by(binary(), binary()) -> notification_doc:notification() | notfound.
-spec find_by_read_by(binary(), binary(), binary()) -> notification_doc:notification() | notfound.
find_by_read_by(Type, Id) ->
	sumo:find_by(notification_doc, [{"read_by#id", Id}, {"read_by#type", Type}]).
find_by_read_by(Type, Id, NotiId) ->
	sumo:find_by(notification_doc, [{"read_by#id", Id}, {"read_by#type", Type}, {id, NotiId}]).

-spec find_all() -> [notification_doc:notification()].
find_all() ->
	sumo:find_all(notification_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [notification_doc:notification()].
find_all(Limit, Offset) ->
	sumo:find_all(notification_doc, [], Limit, Offset).

-spec find_by_conditions(conditions(), conditions(), non_neg_integer(), non_neg_integer()) -> [notification_doc:notification()].
find_by_conditions(AccountQuery, Query, Limit, Offset) -> 
	Conditions = build_query(AccountQuery, Query),
	SortOrders = build_sort([], Query),
	sumo:find_by(notification_doc, Conditions, SortOrders, Limit, Offset).

build_sort([], []) -> 
	[{created_time, desc}];

build_sort(Query, [{Key, Value} | Tail]) when is_list(Query) ->

	Condition = if
        Key == <<"sort_destinations_type">> -> {"destinations#type", Value};
        Key == <<"sort_destinations_id">> -> {"destinations#id", Value};
        Key == <<"sort_target_type">> -> {target_type, Value};
        Key == <<"sort_target_id">> -> {target_id, Value};
        Key == <<"sort_content">> -> {content, Value};
        Key == <<"sort_content_type">> -> {content_type, Value};
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
        Key == <<"filter_id">> -> {id, Value};
        Key == <<"filter_destinations">> -> {destinations, Value};
        Key == <<"filter_target_type">> -> {target_type, Value};
        Key == <<"filter_target_id">> -> {target_id, Value};
        Key == <<"filter_icon">> -> {icon, Value};
        Key == <<"filter_content">> -> {content, Value};
        Key == <<"filter_content_type">> -> {content_type, Value};
        Key == <<"filter_image">> -> {image, Value};
        Key == <<"filter_description">> -> {description, Value};
        Key == <<"filter_read_by">> -> {read_by, Value};
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