-ifndef(APP_HRL).

% Start SOS Request status list %
-define(SOS_REQUEST_STATUS_OPEN,<<"open">>).
-define(SOS_REQUEST_STATUS_VERIFIED,<<"verified">>).
-define(SOS_REQUEST_STATUS_ACCEPTED,<<"accepted">>).
-define(SOS_REQUEST_STATUS_EXECUTING,<<"executing">>).
-define(SOS_REQUEST_STATUS_RESOLVED,<<"resolved">>).
-define(SOS_REQUEST_STATUS_REOPEN,<<"re-open">>).
-define(SOS_REQUEST_STATUS_REJECTED,<<"rejected">>).
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

-define(APP_HRL, 'true').
-endif.