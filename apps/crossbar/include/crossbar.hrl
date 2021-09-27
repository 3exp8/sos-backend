-ifndef(CROSSBAR_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/kz_system_config.hrl").

-include("crossbar_types.hrl").

-define(CONFIG_CAT, <<"crossbar">>).
-define(CROSSBAR_CACHE, 'crossbar_cache').
-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(CB_APPS_STORE_LIST, <<"apps_store/crossbar_listing">>).
-define(APP_NAME, <<"crossbar">>).
-define(APP_VERSION, <<"0.8.0">>).
-define(VERSION_1, <<"v1">>).
-define(VERSION_2, <<"v2">>).
-define(VERSION_SUPPORTED, [?VERSION_1, ?VERSION_2]).
-define(CACHE_TTL, whapps_config:get_integer(<<"crossbar">>, <<"cache_ttl">>, 300)).
-define(CROSSBAR_DEFAULT_CONTENT_TYPE, {<<"application">>, <<"json">>, []}).
-define(CONTENT_PROVIDED, [{'to_json', ?JSON_CONTENT_TYPES}]).
-define(CONTENT_ACCEPTED, [{'from_json', ?JSON_CONTENT_TYPES}
                           ,{'from_form', ?MULTIPART_CONTENT_TYPES}
                           ,{'from_binary', ?CSV_CONTENT_TYPES}
                          ]).
-define(ALLOWED_METHODS, [?HTTP_GET
                          ,?HTTP_POST
                          ,?HTTP_PUT
                          ,?HTTP_GET
                          ,?HTTP_DELETE
                          ,?HTTP_HEAD
                          ,?HTTP_PATCH
                          ,?HTTP_OPTIONS]).

-define(DATESECOND, 24*60*60).
-define(EMAILREGX, "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*\\b").
-define(PHONEREGX, "^(\\+?\\d{1,2})(\\d{9,15})$").
-define(TIMEZONEREGX, "[a-zA-Z_]*\\/[a-zA-Z_\\-]*(\\(UTC[\\+\\-]\\d{1,2}\\:\\d{1,2}\\)){1}$").
-define(TIMEREGX, "^(0[0-9]|1[0-9]|2[0-3]):(?:[012345]\\d):(?:[012345]\\d{0,1}$)").
-define(DATEREGX, "([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]){0,1}$)").
-define(WORKFACTOR, 10).
-define(HOOK_TOKEN, <<"OdR64z28vWY8INr7c97R5VKWb9nM2x0A">>).
-define(CREATE, <<"create">>).
-define(UPDATE, <<"update">>).
-define(DELETE, <<"delete">>).

%SUPPORT TYPE APP
-define(CLIENTCONFIRM, <<"client_confirm">>).
-define(OS_TYPE, [<<"ios">>, <<"android">>]).

-define(ACCEPTED, <<"accepted">>).
-define(DONE, <<"done">>).
-define(REJECTED, <<"rejected">>).
-define(CANCELED, <<"cancel">>).
-define(ACTIVE, <<"active">>).
-define(INACTIVE, <<"inactive">>).
-define(USER_STATUS_ACTIVE, <<"active">>).
-define(USER_STATUS_INACTIVE, <<"inactive">>).
-define(USER_STATUS_UNCONFIRMED, <<"unconfirmed">>).
  
-define(HIDE, <<"hide">>).
-define(SHOW, <<"show">>).
-define(ON, <<"true">>).
-define(OFF, <<"false">>).
-define(CREATE_CONFIRM_RESOUCE, <<"confirm">>).
-define(FORGET_CONFIRM_RESOURCE, <<"password_reset/verify">>).
-define(ENV, [<<"dev">>, <<"prod">>]).
-define(DELIVERED, <<"success">>).
-define(FAIL, <<"failure">>).
-define(PATH_CONFIRM, <<"confirm">>).
-define(PATH_PASSWORD_CHANGE, <<"password_change">>).
-define(PATH_LOGOUT, <<"logout">>).

-define(DEAL_ORDER_CODE_SIZE, 3).
-define(UUID_SIZE, 16).
-define(CODE_SIZE, 2).
-define(SMS_CODE_SIZE, 2).

-define(PORTAL_URL, zt_util:to_bin(application:get_env(crossbar, portal_url, ""))).
-define(EMAIL_FROM_ADDRESS, application:get_env(crossbar, email_from_address, "")).
-define(EMAIL_FROM_NAME, zt_util:to_bin(application:get_env(crossbar, email_from_name, "Support Center"))).
-define(EMAIL_SUPPORT_LIST, application:get_env(crossbar, email_support_lists, [])).


-define(DEFAULT_LIMIT, 20).
-define(DEFAULT_OFFSET, 0).
-define(IS_COUNT, <<"false">>).

%% user roles
-define(USER_ROLE_SYSTEM, <<"SYSTEM">>).

-define(USER_ROLE_CUSTOMER, <<"CUSTOMER">>).  %% unused
-define(USER_ROLE_GUEST, <<"GUEST">>).  %% not loggined user

-define(USER_ROLE_ANY, <<"ANY">>).
-define(USER_ROLE_USER_GE, <<"USER_GE">>).
-define(USER_ROLE_OPERATOR_GE, <<"OPERATOR_GE">>).
-define(USER_ROLE_USER, <<"USER">>).  %% logged in user
-define(USER_ROLE_OPERATOR, <<"OPERATOR">>).  %% opeator user
-define(USER_ROLE_ADMIN, <<"ADMIN">>).  %% admin user
-define(USER_ROLES, [?USER_ROLE_USER,?USER_ROLE_OPERATOR,?USER_ROLE_ADMIN]).  %% admin user

-define(CREATED_SOURCE_APP, <<"customers_app">>).

-define(OBJECT_TYPE_GUEST,<<"guest">>).
-define(OBJECT_TYPE_USER,<<"user">>).
-define(OBJECT_TYPE_GROUP,<<"group">>).
-define(SUGGEST_TARGET_TYPES,[?OBJECT_TYPE_USER, ?OBJECT_TYPE_GROUP]).


-define(REQUESTER_TYPE_GUEST,<<"guest">>).
-define(REQUESTER_TYPE_USER,<<"user">>).
-define(REQUESTER_TYPE_GROUP,<<"group">>).
-define(REQUESTER_TYPES,[?REQUESTER_TYPE_GUEST, ?REQUESTER_TYPE_USER, ?REQUESTER_TYPE_GROUP]).

-define(SHARE_PHONE_NUMBER_TYPE_PRIVATE,<<"private">>).
-define(SHARE_PHONE_NUMBER_TYPE_PUBLIC,<<"public">>).
-define(SHARE_PHONE_NUMBER_TYPES,[?SHARE_PHONE_NUMBER_TYPE_PRIVATE, ?SHARE_PHONE_NUMBER_TYPE_PUBLIC]).

-define (CLUSTER_ACTION_JOIN,<<"join">>).
-define (CLUSTER_ACTION_LEAVE,<<"leave">>).
-define (CLUSTER_ACTION_TYPES,[?CLUSTER_ACTION_JOIN, ?CLUSTER_ACTION_LEAVE]).


%% grant_type
-define(GRANT_TYPE, [<<"password">>, <<"refresh_token">>, <<"client_credentials">>, <<"token">>]).

% end Transaction params
-define(DEFAULT_DATE_TIME, <<"1970-01-01T00:00:00Z">>).

-define(MaxItemPart, 4).

-define(DEFAULT_MODULES,[ 
      %'cb_accounts', 
      'cb_auth', 
      'cb_client', 
      'cb_configuration', 
      'cb_confirm_code', 
      'cb_device', 
      'cb_events', 
      'cb_forgot', 
      'cb_group', 
      'cb_news',
      'cb_notifications',
      'cb_phone_number', 
      'cb_province', 
      'cb_reset',
      'cb_role',
      'cb_search',
      'cb_setting',
      'cb_support',
      'cb_user',
      'cb_sos_request',
      'cb_support_type',
      'cb_support_trans',
      'cb_apis',
      'cb_s3',
      'cb_systemctl',
      'cb_charity_request'
    ]).


-define(DEPRECATED_MODULES, ['cb_local_resources', 'cb_global_resources']).

-define(PIVOT_TYPE, [<<"commands">>, <<"collected">>]).

-define(HTTP_CODE_SUCCESS, 200).
-define(HTTP_CODE_CREATE_SUCCESS, 201).
-define(HTTP_CODE_BAD_REQUEST, 400).
-define(HTTP_CODE_UNAUTHORIZED, 401).
-define(HTTP_CODE_FORBIDDEN, 403).
-define(HTTP_MSG_FORBIDDEN, <<"Forbidden">>).
-define(HTTP_CODE_NOT_FOUND, 404).
-define(HTTP_MSG_NOT_FOUND, <<"Not found">>).
-define(HTTP_CODE_SERVER_ERROR, 500).
-define(HTTP_MSG_SERVER_ERROR, <<"Internal server error">>).

-define(ENV_DEV, <<"development">>).
-define(ENV_PROD, <<"production">>).

-define(DEFAULT_SENDER_PHONE_NUMBER, <<>>).

-define(RAW_TYPE, [<<"recordings">>]).
-define(TO_ATOM(M, BK, V, C), M:(app_util:to_atom(BK))(V, C)).
-define(TO_ATOM(M, BK, Prefix, V, C), M:(app_util:to_atom(BK))(Prefix, V, C)).


-record(cb_context, {
          content_types_provided = [] :: crossbar_content_handlers()
          ,content_types_accepted = [] :: crossbar_content_handlers()
          ,allowed_methods = ?ALLOWED_METHODS :: http_methods()
          ,allow_methods = ?ALLOWED_METHODS :: http_methods()
          ,languages_provided = [<<"en">>, <<"en-us">>, <<"en-gb">>] :: ne_binaries() %% english by default
          ,charsets_provided = [<<"iso-8859-1">>] :: ne_binaries() %% all charsets provided
          ,encodings_provided = [<<"gzip;q=1.0">>,<<"identity;q=0.5">>] :: ne_binaries() %% gzip and identity
          ,auth_token = <<>> :: binary() | 'undefined'
          ,auth_account_id :: api_binary()
          ,auth_doc :: api_object()
          ,req_verb = ?HTTP_GET :: http_method() % see ?ALLOWED_METHODS
          ,req_nouns = [{<<"404">>, []}] :: req_nouns() % {module, [id]} most typical
          ,req_json = wh_json:new() :: req_json()
          ,req_files = [] :: req_files()
          ,req_data :: wh_json:json_term()  % the "data" from the request JSON envelope
          ,req_headers = [] :: cowboy:http_headers()
          ,query_json = wh_json:new() :: wh_json:object()
          ,account_id :: api_binary()
          ,customer_id :: api_binary()
					,id :: api_binary()
					,server_key :: binary()
					,total :: integer()
          ,role :: api_binary()
          ,scope :: api_binary()
          ,roles :: [map()]
          ,user_id :: api_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/users/{user-id}/*
          ,device_id :: api_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/devices/{device-id}/*
          ,reseller_id :: api_binary()
          ,db_name :: api_binary() | ne_binaries()
          ,db_doc :: #{}
          ,doc :: api_object() | wh_json:objects()
          ,resp_expires = {{1999,1,1},{0,0,0}} :: wh_datetime()
          ,resp_etag :: 'automatic' | string() | api_binary()
          ,resp_status = 'error' :: crossbar_status()
          ,resp_error_msg :: wh_json:key()
          ,resp_error_code :: pos_integer()
          ,resp_data :: resp_data()
          ,resp_headers = [] :: wh_proplist() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
          ,resp_envelope = wh_json:new() :: wh_json:object()
          ,resp_extra = [] :: wh_proplist()
          ,start = os:timestamp() :: wh_now()
          ,req_id = ?LOG_SYSTEM_ID :: ne_binary()
          ,storage = [] :: wh_proplist()
          ,raw_host = <<>> :: binary()
          ,port = 8000 :: integer()
          ,raw_path = <<>> :: binary()
          ,raw_qs = <<>> :: binary()
          ,method = ?HTTP_GET :: http_method()
          ,validation_errors = wh_json:new() :: wh_json:object()
          ,client_ip = <<"127.0.0.1">> :: api_binary()
          ,load_merge_bypass :: api_object()
          ,profile_id :: api_binary()
          ,api_version = ?VERSION_1 :: ne_binary()
          ,magic_pathed = 'false' :: boolean()
          ,scopes = [] :: wh_proplist()
          ,context_data :: map()
         }).

-define(CROSSBAR_HRL, 'true').
-endif.
