-module(phone_number_db).

-export([
      save/1, 
	    del/1, 
      del_by_id/1, 
      del_by_phone_number/1, 
      del_all/0, 
	    find/1, 
      find_by_phone_number/1, 
      find_all/0, 
      find_all/2, 
      find_by_conditions/4
    ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

-define(DOC,phone_number_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% User API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new user.

-spec save(?DOC:info()) -> ok.
save(Info) ->
  sumo:persist(?DOC, Info).

%% @doc Deletes the given user.
-spec del(?DOC:info()) -> boolean().
del(Info) ->
  sumo:delete(?DOC, ?DOC:id(Info)).

-spec del_by_id(binary()) -> boolean().
del_by_id(Id) ->
  sumo:delete(?DOC, Id).

-spec del_by_phone_number(binary()) -> boolean().
del_by_phone_number(PhoneNumber) ->
  sumo:delete_by(?DOC, [{phone_number, PhoneNumber}]).

-spec del_all() -> boolean().
del_all() ->
  sumo:delete_all(?DOC).

-spec find(binary()) -> ?DOC:info()|notfound.
find(Id) ->
  sumo:find(?DOC, Id).

-spec find_by_phone_number(binary()) -> [?DOC:info()].
find_by_phone_number(PhoneNumber) ->  
  sumo:find_by(?DOC, [{phone_number, PhoneNumber}]).


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

build_sort([], []) ->
  [{phone_number, asc}];

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) -> 

  Condition = if  
        Key == <<"sort_phone_number">>  -> {phone_number, Value};
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
        Key == <<"filter_phone_number">>  -> {phone_number_id, Value};      
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