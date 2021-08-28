
-record(access_token_mnesia_doc, {
    token :: binary(),
    refresh_token :: binary(),
    user_id :: binary(),
    account_id :: binary(),
    context :: map(),
    roles :: [map()]
    
}).

-type datetime() :: {date(), time()} | binary().
-type date() :: {year(), month(), day()}.
-type year() :: integer().
-type month() :: 1..12 .
-type day() :: 1..31 .
-type time() :: {hour(), minute(), second()} .
-type hour() :: 0..23 .
-type minute() :: 0..59 .
-type second() :: 0..59 .

-define(TimeZoneDefault, <<"Asia/Jakarta(UTC+07:00)">>).
-define(TimeZoneUTCDefault, <<"(UTC+7:00)">>).
