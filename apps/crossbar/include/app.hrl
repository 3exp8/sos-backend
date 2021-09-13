-ifndef(APP_HRL).

% Start SOS Request status list %
-define(SOS_REQUEST_STATUS_OPEN,<<"open">>).
-define(SOS_REQUEST_STATUS_VERIFIED,<<"verified">>).
-define(SOS_REQUEST_STATUS_ACCEPTED,<<"accepted">>).
-define(SOS_REQUEST_STATUS_EXECUTING,<<"executing">>).
-define(SOS_REQUEST_STATUS_RESOLVED,<<"resolved">>).
-define(SOS_REQUEST_STATUS_REOPEN,<<"re-open">>).
-define(SOS_REQUEST_STATUS_REJECTED,<<"rejected">>).

-define(SOS_REQUEST_STATUS_LIST,[
    ?SOS_REQUEST_STATUS_OPEN,
    ?SOS_REQUEST_STATUS_VERIFIED,
    ?SOS_REQUEST_STATUS_ACCEPTED,
    ?SOS_REQUEST_STATUS_EXECUTING,
    ?SOS_REQUEST_STATUS_RESOLVED,
    ?SOS_REQUEST_STATUS_REOPEN,
    ?SOS_REQUEST_STATUS_REJECTED
]).

% End SOS Request status list %

% Start SOS Task status list %
-define(SOS_TASK_STATUS_OPEN,<<"open">>).
-define(SOS_TASK_STATUS_EXECUTING,<<"executing">>).
-define(SOS_TASK_STATUS_RESOLVED,<<"resolved">>).
-define(SOS_TASK_STATUS_PENDING,<<"pending">>).
-define(SOS_TASK_STATUS_CANCELED,<<"canceled">>).
% End SOS Task status list %

% Start SOS Request suggest status list %
-define(SOS_REQUEST_SUGGEST_STATUS_OPEN,<<"open">>).
-define(SOS_REQUEST_SUGGEST_STATUS_ACCEPTED,<<"accepted">>).
-define(SOS_REQUEST_SUGGEST_STATUS_REJECTED,<<"rejected">>).
% End SOS Request suggest status list %

-define(SOS_REQUEST_TYPE_ASK,<<"ask">>).
-define(SOS_REQUEST_TYPE_OFFER,<<"offer">>).
-define(SOS_REQUEST_TYPES, [?SOS_REQUEST_TYPE_ASK,?SOS_REQUEST_TYPE_OFFER]).


-define(APP_HRL, 'true').
-endif.