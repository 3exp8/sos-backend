-module(client_db).

-export([ new_client/1, save_client/1, save/1, 
		  del_client/1, del_client_by_key/1, 
      del_all_clients/0, find_client/1, find/1,
      find_client_by_email/1, find_all_clients/0, 
      find_all_clients/2, find_by_conditions/4,
      reindex/0]).


-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new_client(binary()) -> client_doc:client().
new_client({ClientId, SecretKey, AccountId, AccountRole, ClientName, Email, Logo, UrlInfo, 
  Description, RedirectUri, CreatedBy, CreatedTime, UpdatedBy, UpdatedTime}) ->
  sumo:persist(client_doc, client_doc:new({ClientId, SecretKey, AccountId, AccountRole, ClientName, Email, Logo, 
    UrlInfo, Description, RedirectUri, CreatedBy, CreatedTime, UpdatedBy, UpdatedTime})).

-spec save_client(client_doc:client()) -> ok.
save_client(Client) ->
  sumo:persist(client_doc, Client).

-spec save(client_doc:client()) -> ok.
save(Client) ->
  sumo:persist(client_doc, Client).  

-spec del_client(client_doc:client()) -> boolean().
del_client(Client) ->
  sumo:delete(client_doc, client_doc:client_key(Client)).

-spec del_client_by_key(binary()) -> boolean().
del_client_by_key(ClientKey) ->
  sumo:delete(client_doc, ClientKey).

-spec del_all_clients() -> boolean().
del_all_clients() ->
  sumo:delete_all(client_doc).

-spec find_client(binary()) -> client_doc:client()|notfound.
find_client(ClientKey) ->
  sumo:find(client_doc, ClientKey).

-spec find(binary()) -> client_doc:client()|notfound.
find(ClientKey) ->
  sumo:find(client_doc, ClientKey).

-spec find_client_by_email(binary()) -> client_doc:client() | notfound.
find_client_by_email(Email) ->
  sumo:find_by(client_doc, [{email, Email}]).

-spec find_all_clients() -> [client_doc:client()].
find_all_clients() ->
  sumo:find_all(client_doc).

-spec find_all_clients(non_neg_integer(), non_neg_integer()) -> [client_doc:client()].
find_all_clients(Limit, Offset) ->
  sumo:find_all(client_doc, [], Limit, Offset).


-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [client_doc:client()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query), 
  SortOrders = build_sort([], Query),
  sumo:find_by(client_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if  
        Key == <<"sort_client_id">> -> {client_id, Value};
        Key == <<"sort_account_id">>  -> {account_id, Value};
        Key == <<"sort_account_role">>  -> {account_role, Value};
        Key == <<"sort_client_name">>  -> {client_name, Value};
        Key == <<"sort_email">>  -> {email, Value};
        Key == <<"sort_logo">>  -> {logo, Value};
        Key == <<"sort_url_info">> -> {url_info, Value};
        Key == <<"sort_description">> -> {description, Value};
        Key == <<"sort_created_by">> -> {created_by, Value};
        Key == <<"sort_created_time">> -> {created_time_dt, Value};
        Key == <<"sort_updated_by">> -> {updated_by, Value};
        Key == <<"sort_updated_time">> -> {updated_time_dt, Value};
        true -> ignore
      end, 
  lager:debug("------ build_sort: Condition ~p ~n",[Condition]),
  NewQuery = 
  case Condition of
    ignore ->
      Query;
    _ ->
      [Condition|Query]
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
        Key == <<"filter_created_time_gt">>  -> {created_time_dt, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_gte">>  -> {created_time_dt, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lt">>  -> {created_time_dt, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lte">>  -> {created_time_dt, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time">>  -> {created_time_dt, datetime_util:utc_format(Value)};
        Key == <<"filter_client_id">> -> {client_id, Value};
        Key == <<"filter_account_role">>  -> {account_role, Value};
        Key == <<"filter_client_name">>  -> {client_name, Value};
        Key == <<"filter_email">>  -> {email, Value};
        true -> ignore
      end, 
  lager:debug("------ build_query: Condition ~p ~n",[Condition]),
  NewQuery =
  case Condition of
    ignore ->
      Query;
    _ ->
      [Condition|Query]
  end,
  lager:debug("------ build_query: NewQuery ~p ~n",[NewQuery]),
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query ;

build_query(_Query, _Other) ->
  [].

reindex()->
  Docs = find_all_clients(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  save_client(H),
  reindex(T).    