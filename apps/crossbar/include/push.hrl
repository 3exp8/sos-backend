-define(TTL, 86400).
-define(PRIORITIZE, <<"high">>).

-define(IOS, <<"ios">>).
-define(ANDROID, <<"android">>).
-define(UNKNOW, <<"unknow">>).

-define(DEVICE_TYPES, [<<"webapp">>, <<"ios">>, <<"android">>]).
-define(WEBAPP, <<"webapp">>).
-define(ACTIVE_DEVICE, <<"active">>).
-define(INACTIVE_DEVICE, <<"inactive">>).
-define(FCM_API_URL,application:get_env(mpark, fcm_api_url, "")).
-define(FCM_AUTH_KEY, application:get_env(mpark, fcm_auth_key, "")).

%--Destinations--%
-define(PUSH_DES_TYPE_CUSTOMER, <<"customer">>).
-define(PUSH_DES_TYPE_STATION, <<"station">>).
-define(PUSH_DES_TYPE_BRANCH, <<"branch">>).
-define(PUSH_DES_TYPE_ACCOUNT, <<"account">>).
-define(PUSH_DES_TYPE_USER, <<"user">>).
