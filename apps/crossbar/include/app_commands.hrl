
-record(app_commands,
        {name                    :: atom(),
         tags = []               :: [atom()] | '_' | '$2',
         desc = ""               :: string() | '_' | '$3',
         longdesc = ""           :: string() | '_',
         version = 0             :: integer(),
         weight = 1              :: integer(),
         module                  :: atom() | '_',
         function                :: atom() | '_',
         args = []               :: [term()] | '_' | '$1' | '$2',
         policy = restricted     :: open | restricted | admin | user,
        %% access is: [accessRuleName] or [{Module, AccessOption, DefaultAccessRuleName}]
         access = []             :: [{atom(),atom(),atom()}|atom()],
         result = {res, rescode} :: term() | '_' | '$2',
         args_desc = none        :: none | [string()] | '_',
         result_desc = none      :: none | string() | '_',
         args_example = none     :: none | [any()] | '_',
         result_example = none   :: any()}).


-define(STATUS_SUCCESS, 0).
-define(STATUS_ERROR,   1).
-define(STATUS_USAGE,   2).
-define(STATUS_BADRPC,  3).


%% TODO Fix me: Type is not up to date
-type app_commands() :: #app_commands{name :: atom(),
                                                tags :: [atom()],
                                                desc :: string(),
                                                longdesc :: string(),
                                                version :: integer(),
                                                module :: atom(),
                                                function :: atom(),
                                                args :: [term()],
                                                policy :: open | restricted | admin | user,
                                                access :: [{atom(),atom(),atom()}|atom()],
                                                result :: term()}.
