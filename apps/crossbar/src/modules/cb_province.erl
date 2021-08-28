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

-export([init/0
	,validate/1
	,validate/2
    ,validate/3
    ,validate/4
	,resource_exists/0
	,resource_exists/1
    ,resource_exists/2
    ,resource_exists/3
	,authenticate/1
	,authenticate/2
    ,authenticate/3
    ,authenticate/4
	,authorize/1
    ,authorize/2
    ,authorize/3
    ,authorize/4
	,allowed_methods/0
	,allowed_methods/1
    ,allowed_methods/2
    ,allowed_methods/3
	,handle_post/2
	,handle_get/1
	,handle_get/2
    ,handle_get/3
    ,handle_get/4
]).

-export([
    find_district_by_code/2,
    find_Ward_by_code/2,
    get_province_district_ward_info/3,
    get_address_detail_info/1
]).
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

allowed_methods(_Id,_Id2) ->
	[?HTTP_GET].

allowed_methods(_Id,_Id2,_Id3) ->
	[?HTTP_GET].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(),path_token()) -> 'true'.
-spec resource_exists(path_token(),path_token(),path_token()) -> 'true'.

%% /api/v1/demo
resource_exists() -> 'true'.

%% /api/v1/demo/{id}
resource_exists(_Id) -> 'true'.

resource_exists(_Id1, _Id2) -> 'true'.

resource_exists(_Id, _Id2, _Id3) -> 'true'.


-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
-spec authenticate(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().

authenticate(Context) -> 
	true.
	
authenticate(Context, _Id) -> true.
authenticate(Context, _Id1,_Id2) -> true.

authenticate(Context, _Id,_Id2,_Id3) -> true.


-spec authorize(cb_context:context()) -> boolean().
-spec authorize(cb_context:context(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
-spec authorize(cb_context:context(), path_token(), path_token(), path_token()) -> boolean().

authorize(_Context) ->
    true.

authorize(_Context, _Id) -> true. 
authorize(_Context, _Id,_Id1) -> true. 
authorize(_Context, _Id,_Id1,_Id2) -> true. 

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/test
validate(Context) ->
    validate_test(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/test/{id}
validate(Context, Id) ->
    validate_test(Context, Id, cb_context:req_verb(Context)).   

validate(Context, Id, Id2) ->
    validate_test(Context, Id, cb_context:req_verb(Context)). 

validate(Context, Id, Id2, Id3) ->
    validate_test(Context, Id, cb_context:req_verb(Context)). 

%% GET /api/v1/test
validate_test(Context, ?HTTP_GET = Verb) ->
	validate_request(Context, Verb).
	
%% GET /api/v1/test/{id}
validate_test(Context, Id, ?HTTP_GET = Verb) ->
	validate_request(Id, Context, Verb);

validate_test(Context, Id, ?HTTP_POST = Verb) ->
	validate_request(Id, Context, Verb).

get_sub_fields_provinces(Info) -> 
Fields = [districts] ,
maps:without(Fields, Info).

get_sub_fields_districts(Info) -> 
Fields = [<<"wards">>] ,
maps:without(Fields, Info).

handle_get({Req, Context}) ->
	lager:info("--------- Get provinces 1 ~n",[]),
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
	Provinces = province_db:find_by_conditions([],PropQueryJson,Limit,Offset),
    lager:info("--------- Get provinces 2 ~n",[]),
    PropProvinces = lists:map(fun(Info) ->
        get_sub_fields_provinces(Info) end, 
        Provinces),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, PropProvinces}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]),
	{Req, Context1}.

handle_get({Req, Context},Id) ->
	lager:info("--------- get province by id 1: ~p ~n",[Id]),
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
            PropDistricts = lists:map(fun(Info) ->
                get_sub_fields_districts(Info) end, Districts),
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

find_district_by_code([], _Code) -> notfound;
find_district_by_code([#{<<"code">> := Code} = Info|DistrictList], Code) -> Info;

find_district_by_code([Info|WardList], Code) -> 
    lager:debug("find_Ward_by_code ~p~n",[Info]),
    find_district_by_code(WardList,Code).
        
find_Ward_by_code([], _Code) -> notfound;
find_Ward_by_code([#{<<"code">> := Code} = Info|WardList], Code) -> Info;

find_Ward_by_code([Info|WardList], Code) -> 
    lager:debug("find_Ward_by_code ~p~n",[Info]),
    find_Ward_by_code(WardList,Code).

handle_get({Req, Context},Id,DistrictCode) ->
	lager:info("--------- get wards by id 1: ~p, DistrictCode: ~p ~n",[Id,DistrictCode]),
    #{districts := Districts} = province_db:find(Id),
    Code = zt_util:to_integer(DistrictCode),
    Context1 = 
    case find_district_by_code(Districts,Code) of 
    notfound -> 
            cb_context:setters(Context,
            [{fun cb_context:set_resp_error_msg/2, <<"District Not Found">>},
            {fun cb_context:set_resp_status/2, <<"error">>},
            {fun cb_context:set_resp_error_code/2, 404}
            ]);
    Info -> 
        lager:info("--------- Get districts by id 2 ~n",[]),
	    cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, Info}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ])
    end,
	{Req, Context1}.

handle_get({Req, Context},Id,Id1,Id2) ->
	lager:info("--------- get province by id 1: ~p ~n",[Id]),
    Info = province_db:find(Id),
	lager:info("--------- Get provinces by id 2 ~n",[]),
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, Info}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]),
	{Req, Context1}.
handle_post(Context, <<"province">>) ->
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
	% Data = 
	% 	lists:foreach(fun(RecordInfo) -> 
	% 	lager:debug("record info: ~p~n",[RecordInfo])
	% 	end,Records),
	
	cb_context:setters(Context
						   ,[{fun cb_context:set_resp_data/2, Res}
							 ,{fun cb_context:set_resp_status/2, 'success'}
							]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_address_detail_info(AddressProps) when is_list(AddressProps) -> 
    get_address_detail_info(zt_util:to_map(AddressProps));

get_address_detail_info(ReqAddressInfoRaw) -> 
    ReqAddressInfo = zt_util:map_keys_to_atom(ReqAddressInfoRaw),
    ReqAddressInfoDb = 
        cb_province:get_province_district_ward_info(
            maps:get(province_id, ReqAddressInfo,<<>>),
            maps:get(district_code, ReqAddressInfo, 0),
            maps:get(ward_code, ReqAddressInfo, 0)
        ),
     maps:merge(ReqAddressInfoDb, #{
        address => maps:get(address, ReqAddressInfo,<<>>)
    }).

get_province_district_ward_info(<<>>, _DistrictCodeStr, _WardCodeStr) -> #{};

get_province_district_ward_info(ProvinceId, DistrictCodeStr, WardCodeStr) -> 
    case province_db:find(ProvinceId) of 
        notfound -> #{};
        #{
            name := ProvinceName,
            districts := Districts 
        } -> 
            ProvinceInfo = #{
                province_id => ProvinceId,
                province_name => ProvinceName
            },
            DistrictCode = zt_util:to_integer(DistrictCodeStr),
            case find_district_by_code(Districts,DistrictCode) of 
                notfound -> ProvinceInfo;
                #{
                    <<"name">> := DistrictName,
                    <<"wards">> := Wards
                } -> 
                    DistrictInfo = maps:merge(ProvinceInfo, #{
                        district_code => DistrictCode,
                        district_name => DistrictName
                    }), 
                    WardCode = zt_util:to_integer(WardCodeStr),
                    case find_district_by_code(Wards,WardCode) of 
                        notfound -> DistrictInfo;
                        #{
                            <<"name">> := WardName
                        } -> 
                            maps:merge(DistrictInfo, #{
                                ward_code => WardCode,
                                ward_name => WardName
                            })
                    end
            end
    end.

					   
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