-module(user_db).

%%% User API
-export([
      save/1, 
		  del/1, 
      del_by_id/1, 
      del_by_email/1,
      del_by_phone_number/1, 
      del_all/0, 
		  find/1, 
      find_by_account_id/1, 
      find_by_email/1, 
      find_by_phone_number/1, 
      find_by_role/1, 
      find_all/0, 
      find_all/2, 
      find_by_conditions/4, 
      reindex/0, 
      reindex/1 
    ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% User API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new user.

-spec save(user_doc:user()) -> ok.
save(User) ->
  sumo:persist(user_doc, User).

%% @doc Deletes the given user.
-spec del(user_doc:user()) -> boolean().
del(User) ->
  sumo:delete(user_doc, user_doc:id(User)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(user_doc, Id).

-spec del_by_email(binary()| [binary()]) -> boolean().
del_by_email(Email) when is_binary(Email) ->
  sumo:delete_by(user_doc, [{email_id, Email}]);
  
del_by_email([Email]) ->
  sumo:delete_by(user_doc, [{email_id, Email}]);

del_by_email(Emails) ->
   sumo:delete_by(user_doc, [{email_id,'in',Emails}]).


-spec del_by_phone_number(binary()) -> boolean().
del_by_phone_number(PhoneNumber) ->
  sumo:delete_by(user_doc, [{phone_number_id, PhoneNumber}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(user_doc).

-spec find(binary()) -> user_doc:user()|notfound.
find(Id) ->
  sumo:find(user_doc, Id).

-spec find_by_account_id(binary()) -> [user_doc:user()].
find_by_account_id(AccountId) ->  
  sumo:find_by(user_doc, [{account_id, AccountId}]).

-spec find_by_email(binary()) -> [user_doc:user()].
find_by_email(Email) ->  
  sumo:find_by(user_doc, [{email_id, Email}]).

-spec find_by_phone_number(binary()) -> [user_doc:user()].
find_by_phone_number(PhoneNumber) ->  
  sumo:find_by(user_doc, [{phone_number_id, PhoneNumber}]).

-spec find_by_role(binary()) -> [user_doc:user()].
find_by_role(Role) ->  
  sumo:find_by(user_doc, [{role_id, Role}]).

-spec find_all() -> [user_doc:user()].
find_all() ->
  sumo:find_all(user_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [user_doc:user()].
find_all(Limit, Offset) ->
  sumo:find_all(user_doc, [], Limit, Offset).


-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [user_doc:user()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  sumo:find_by(user_doc, Conditions, SortOrders, Limit, Offset).

build_sort([], []) ->
  [{first_name, asc}];

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  Condition = if  
        Key == <<"sort_email">>  -> {email_id, Value};
        Key == <<"sort_first_name">>  -> {first_name, Value};
        Key == <<"sort_last_name">>  -> {last_name, Value};
        Key == <<"sort_role">>  -> {role_id, Value};
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
        Key == <<"filter_account_id">> -> {account_id, Value};
        Key == <<"filter_email">>  -> {email_id, Value};
        Key == <<"filter_phone">>  -> {phone_number_id, Value};
        Key == <<"filter_first_name">>  -> {first_name, Value};
        Key == <<"filter_last_name">>  -> {last_name, Value};
        Key == <<"filter_role">>  -> {role_id, Value};
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