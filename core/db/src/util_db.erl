-module (util_db).
-export([to_float/1
        ,now_to_utc_binary/1
        ,now_to_utc_string/1
        ,parse_date/1
        ,arr/1
        ,to_bin/1
        ,to_integer/1
        ,to_str/1
        ,to_atom/1
        ,price_enformat/1
        ,price_deformat/1
        ,enformat_price_field/1
        ,deformat_price_field/1
        ,fortmat_recipe/1
        ,trans_props/1]).

now_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    % lists:flatten(
    %   io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
    %                 [Year, Month, Day, Hour, Minute, Second, MicroSecs]));
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                    [Year, Month, Day, Hour, Minute, Second]));

now_to_utc_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    lists:flatten(
      io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                    [Year, Month, Day, Hour, Minute, Second])).


now_to_utc_binary(Time) ->
    list_to_binary(now_to_utc_string(Time)).

%% @doc yyyy-mm-dd
parse_date(Date) when is_binary(Date) ->
    parse_date(binary_to_list(Date));
parse_date(Date) ->
    [Y, M, D] = string:tokens(Date, "-"),
    Date1 = {list_to_integer(Y), list_to_integer(M), list_to_integer(D)},
    case calendar:valid_date(Date1) of
        true ->
            Date1;
        _ ->
            false
    end.

     
to_float(Amount) when is_binary(Amount) ->
  case catch binary_to_float(Amount) of
  Val  when is_number(Val) ->
    Val ;
  _ ->
    case catch binary_to_integer(Amount) of 
      Val when is_number(Val) ->
        float(Val);
    _ ->
     	0.0
    end
  end;

to_float(Amount) when is_list(Amount) ->
  case catch list_to_float(Amount) of 
    Val when is_number(Val) ->
      Val ;
    _ ->
      case catch list_to_integer(Amount) of 
        Val when is_number(Val) ->
          float(Val);
        _ ->
          0.0
      end
  end;

to_float(Amount) when is_integer(Amount) ->
  float(Amount) ;

to_float(Amount) when is_atom(Amount) ->
  AmountStr = atom_to_list(Amount),
  to_float(AmountStr);

to_float(Amount) ->
  Amount. 


arr([<<>>]) ->
  [];
arr(<<>>) ->
  [];
arr(Val) when is_binary(Val) ->
  [Val] ;
arr(Val) ->
  Val. 
  

%% @private
% to_bin([H|_] = Vals) ->
%   [to_bin(Val) || Val <- Vals];
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data, [{decimals, 4}, compact]);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) ->
  Data.

to_str(Data) when is_binary(Data) ->
  binary_to_list(Data) ;
to_str(Data) when is_integer(Data) ->
  integer_to_list(Data) ;
to_str(Data) when is_float(Data) ->
  float_to_list(Data, [{decimals, 4}, compact]) ;
to_str(Data) when is_atom(Data) ->
  atom_to_list(Data) ;
to_str(Data) ->
  Data.

%% @private
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

to_integer(Val) when is_binary(Val) ->
  binary_to_integer(Val);
to_integer(Val) when is_list(Val) ->
  list_to_integer(Val);
to_integer(Val) when is_float(Val) ->
  round(Val);
to_integer(Val) ->
  Val.


-spec price_enformat(map() | [tuple()]) -> map().
price_enformat(Price) ->
  PricSet = enformat_price_field(Price), 
  Id = proplists:get_value(<<"id">>, PricSet, <<>>),
  Name =proplists:get_value(<<"name">>, PricSet, <<>>),
  PriceVal = proplists:get_value(<<"price">>, PricSet, <<>>),
  CostVal = proplists:get_value(<<"cost">>, PricSet, <<>>),
  Sku = proplists:get_value(<<"sku">>, PricSet, <<>>),
  Recipes = proplists:get_value(<<"recipes">>, PricSet, []),
  Quantity = proplists:get_value(<<"quantity">>, PricSet, 0),
  lager:debug("Recipes: ~p~n",[Recipes]),
  RecipesFormat = 
  lists:filtermap(fun(RecipeData)->

    {
      true, 
      fortmat_recipe(RecipeData)
    }
  end, Recipes),

  #{
      id_arr => Id, 
      name_arr => Name, 
      price_arr => to_bin(PriceVal), 
      cost_arr => to_bin(CostVal), 
      sku_arr => to_bin(Sku),
      quantity => Quantity,
      recipes => RecipesFormat
    }.


-spec price_deformat(map() | [tuple()]) -> map().
price_deformat(Price) ->
    PriceProps = deformat_price_field(Price),
    maps:from_list(PriceProps).

fortmat_recipe(RecipeList) when is_list(RecipeList)->
 fortmat_recipe(maps:from_list(RecipeList));

fortmat_recipe(RecipeMap) when is_map(RecipeMap)->
  #{
      inventory_id => maps:get(<<"inventory_id">>, RecipeMap, <<>>),
      inventory_code => maps:get(<<"inventory_code">>, RecipeMap, <<>>),
      inventory_name => maps:get(<<"inventory_name">>, RecipeMap, <<>>),
      amount => zt_util:to_float(maps:get(<<"amount">>, RecipeMap, 0)),
      amount_unit => maps:get(<<"amount_unit">>, RecipeMap, <<>>)
    }.

enformat_price_field(Price) ->
  PriceProps = trans_props(Price),
  proplists:substitute_aliases([
      {id, <<"id">>},
      {name, <<"name">>}, 
  		{price, <<"price">>}, 
      {cost, <<"cost">>}, 
      {sku, <<"sku">>},
      {quantity, <<"quantity">>}
    ], PriceProps).

deformat_price_field(Price) ->
    PriceProps = trans_props(Price),
    Res = proplists:substitute_aliases([
        {id_arr, <<"id">>},
        {name_arr, <<"name">>}, 
  		  {price_arr, <<"price">>}, 
        {cost_arr, <<"cost">>}, 
        {sku_arr, <<"sku">>}, 
       {quantity_arr, <<"quantity">>}, 
        {<<"id_arr">>, <<"id">>},
  		  {<<"name_arr">>, <<"name">>}, 
        {<<"price_arr">>, <<"price">>}, 
        {<<"cost_arr">>, <<"cost">>}, 
      {<<"sku_arr">>, <<"sku">>},
      {<<"quantity_arr">>, <<"quantity">>}
      ], PriceProps).   

trans_props(MapVal) when is_map(MapVal) ->
        maps:to_list(MapVal);

trans_props(Val) ->
        Val. 