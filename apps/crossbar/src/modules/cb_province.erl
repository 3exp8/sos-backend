%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API test demo
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_province).

-include("crossbar.hrl").

-export([
      init/0
	,validate/1
	,validate/2
    ,validate/3
	,resource_exists/0
	,resource_exists/1
    ,resource_exists/2
	,authenticate/1
	,authenticate/2
    ,authenticate/3
	,authorize/1
    ,authorize/2
    ,authorize/3
	,allowed_methods/0
	,allowed_methods/1
    ,allowed_methods/2
	,handle_post/2
	,handle_get/1
	,handle_get/2
    ,handle_get/3
]).

-define(PATH_PROVINCE,<<"province">>).
init() ->
	_ = crossbar_bindings:bind(<<"*.resource_exists.provinces">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.provinces">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authenticate.provinces">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.provinces">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.provinces">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.provinces">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.provinces">>, ?MODULE, 'content_types_accepted'), 
	_ = crossbar_bindings:bind(<<"*.execute.post.provinces">>, ?MODULE, 'handle_post'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.provinces">>, ?MODULE, 'handle_get').


%% /api/v1/demo
allowed_methods() ->
	[?HTTP_GET].

%% /api/v1/demo/{id}
allowed_methods(_Id) ->
	[?HTTP_GET, ?HTTP_POST].

allowed_methods(_Id,_Path) ->
	[?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(),path_token()) -> 'true'.

%% /api/v1/demo
resource_exists() -> 'true'.

%% /api/v1/demo/{id}
resource_exists(_Id) -> 'true'.
resource_exists(_Id,_Path) -> 'true'.


-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().

authenticate(Context) -> 
	true.
	
authenticate(Context, Id) -> 
    authenticate_verb(Context, Id, cb_context:req_verb(Context)).

authenticate_verb(_Context, _Id, ?HTTP_GET) -> true;

authenticate_verb(Context, _Id, _) -> 
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

authenticate(_Context, _Id, _) ->  true.

-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().

authorize(_Context) ->
    true.

authorize(Context, Id) ->  
    authorize_verb(Context, Id, cb_context:req_verb(Context)).

authorize_verb(_Context, _Id, ?HTTP_GET) -> true;

authorize_verb(Context, _Id, _) -> 
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_ADMIN).

authorize(_Context, _Id,_Path) -> true. 

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/test
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/test/{id}
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).   

validate(Context, Id, Path) ->
    validate_request(Id, Path, Context, cb_context:req_verb(Context)). 


handle_get({Req, Context}) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    IsFull = wh_json:get_value(<<"is_full">>, QueryJson, <<"false">>),
    PropQueryJson = wh_json:to_proplist(QueryJson),
    Conds = 
        case IsFull of 
            <<"true">> -> [];
            _ -> [{status,<<"active">>}]
        end,
	Provinces = province_db:find_by_conditions(Conds,[{<<"sort_order">>, asc}|PropQueryJson],Limit,Offset),
    PropProvinces = lists:map(fun(Info) ->
            get_sub_fields_provinces(Info) end, 
        Provinces),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, PropProvinces}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]),
	{Req, Context1}.

handle_get({Req, Context},Id) ->
    Context1 = 
    case province_db:find(Id) of 
    notfound -> 
            cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, <<"Province Not Found">>},
            {fun cb_context:set_resp_status/2, <<"error">>},
            {fun cb_context:set_resp_error_code/2, 404}
            ]);
    Info -> 

            Districts = maps:get(districts,Info,[]),
            PropDistricts = lists:map(fun(DistrictInfo) ->
                get_sub_fields_districts(DistrictInfo) end, Districts),

            NewInfo = maps:merge(Info, #{
                districts => PropDistricts
            }),
            lager:info("--------- Get provinces by id 2 ~n",[]),
            cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ])
    end,
	{Req, Context1}.


handle_get({Req, Context},Id,DistrictCode) ->
	lager:info("--------- get wards by id 1: ~p, DistrictCode: ~p ~n",[Id,DistrictCode]),
    #{districts := Districts} = province_db:find(Id),
    Code = zt_util:to_integer(DistrictCode),
    Context1 = 
    case province_handler:find_district_by_code(Districts,Code) of 
    notfound -> 
            cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, <<"District Not Found">>},
            {fun cb_context:set_resp_status/2, <<"error">>},
            {fun cb_context:set_resp_error_code/2, 404}
            ]);
    Info -> 
	    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, Info}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ])
    end,
	{Req, Context1}.

handle_post(Context, ?PATH_PROVINCE) ->
	ReqJson =  cb_context:req_json(Context),
	Records = wh_json:get_value(<<"data">>, ReqJson, []),
	lager:debug("record length: ~p~n",[length(Records)]),
	[FirstProvince|_] =  Records,
	lager:debug("FirstProvince: ~p~n",[FirstProvince]),
	Res = zt_util:to_map(FirstProvince),
    Res1 = maps:without([districts], Res),
	Info = maps:merge(Res1,#{
		id => zt_util:get_uuid()
	}),
	province_db:save(Info),
	
	cb_context:setters(Context
						   ,[{fun cb_context:set_resp_data/2, Res}
							 ,{fun cb_context:set_resp_status/2, 'success'}
							]);

 handle_post(Context, Id) ->
	ReqJson =  cb_context:req_json(Context),

    case province_db:find(Id)  of 
    #{
        status := StatusDb,
        order := OrderDb,
        default_location := DefaultLocationDb
    } = Info -> 
        NewInfo = maps:merge(Info, #{
            status => wh_json:get_value(<<"status">>, ReqJson, StatusDb),
            order => wh_json:get_value(<<"order">>, ReqJson, OrderDb),
            default_location => wh_json:get_value(<<"default_location">>, ReqJson, DefaultLocationDb)
        }),
        province_db:save(NewInfo),
        Res = get_sub_fields_provinces(NewInfo),
	    cb_context:setters(Context
						   ,[{fun cb_context:set_resp_data/2, Res}
							 ,{fun cb_context:set_resp_status/2, 'success'}
							]);
    _ -> 
        cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"Id Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            )
end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


					   
validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]) ;
					   
validate_request(Context, _Verb) ->
	Context.
					   
validate_request(_Id, Context, ?HTTP_GET) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_POST) ->
	cb_context:setters(Context
		,[{fun cb_context:set_resp_status/2, 'success'}]);


validate_request(_Id, Context, _Verb) ->
	Context.

validate_request(_Id, _Path, Context, ?HTTP_GET) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]).


get_sub_fields_provinces(Info) -> 
    Fields = [districts] ,
    maps:without(Fields, Info).

get_sub_fields_districts(Info) -> 
    Fields = [<<"wards">>] ,
    maps:without(Fields, Info).