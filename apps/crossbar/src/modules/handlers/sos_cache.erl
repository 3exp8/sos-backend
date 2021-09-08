-module(sos_cache).
-export([
     init/0,
     create_table/0,
     set/2,
     get/1
     ]).

-define(CACHE_TABLE, soscahe).
-define(DEFAULT_TTL, 120).
-define(CLEANUP_INTERVAL, 3600).

-record(soscahe, {key, value, ttl}).

init() ->
     mnesia:create_schema([node()]),
     mnesia:start().

create_table() ->
     mnesia:create_table(soscahe, [{attributes, record_info(fields, soscahe)}]).

set(Key, Value) ->
     Insert = fun(Record) -> mnesia:write(?CACHE_TABLE, Record, write) end,
     Expire = erlang:system_time(second) + ?DEFAULT_TTL,
     {atomic, ok} = mnesia:transaction(Insert, [#soscahe{key=Key, value=Value, ttl=Expire}]),
     {ok}.

get(Key) ->
     Read = fun(RKey) -> mnesia:read(?CACHE_TABLE, RKey, read) end,
     Delete = fun(DKey) -> mnesia:delete(?CACHE_TABLE, DKey, write) end,
     {atomic, Result} = mnesia:transaction(Read, [Key]),
     case Result of
          [{_TableName, Key, Value, infinity}]
               -> {ok, Value};
          [{_TableName, Key, Value, Expire}]
               ->
               Now = erlang:system_time(second),
			if
				Expire < Now ->
					{atomic, ok} = mnesia:transaction(Delete, [Key]),
					{error, not_found};
				true ->
					{ok, Value}
			end;
          [] ->
               {error, not_found}
     end.