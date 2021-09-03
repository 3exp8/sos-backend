%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API manage tax code
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_s3).

-include("crossbar.hrl").


-export([init/0
	,validate/1
	,validate/2
	,resource_exists/0
	,resource_exists/1
	,authenticate/1
	,authenticate/2
	,authorize/1
	,authorize/2
	,allowed_methods/0
	,allowed_methods/1
	,handle_get/1
	,handle_get/2

	]).

-define(PATH_PRESIGNED,<<"presigned">>).

init() ->
	_ = crossbar_bindings:bind(<<"*.resource_exists.s3">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.s3">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.s3">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.authorize.s3">>, ?MODULE, 'authorize'),
	_ = crossbar_bindings:bind(<<"*.allowed_methods.s3">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.s3">>, ?MODULE, 'handle_get').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
	[].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
	[?HTTP_GET].

-spec resource_exists() -> 'true'.

%% /api/v1/s3/{id}
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.

%% /api/v1/s3/{id}
resource_exists(_) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().

authenticate(Context) ->  true.


-spec authenticate(cb_context:context(),path_token()) -> boolean().

authenticate(Context,_) ->  true.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) -> true.

-spec authorize(cb_context:context(),path_token()) -> boolean().
authorize(Context,_) -> true.

-spec validate(cb_context:context() ) ->  cb_context:context().

%% Validate resource : /api/v1/tax_codes/{id}
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).   


-spec validate(cb_context:context(),path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/tax_codes/{id}
validate(Context,Path) ->
	validate_request(Context, Path, cb_context:req_verb(Context)).  

handle_get({Req, Context}) -> 
QueryJson = cb_context:query_string(Context),
{Req, 
cb_context:setters(Context,[
    {fun cb_context:set_resp_data/2, QueryJson},
    {fun cb_context:set_resp_status/2, 'success'}
])}.

handle_get({Req, Context}, ?PATH_PRESIGNED) -> 
    QueryJson = cb_context:query_string(Context),
    Path = wh_json:get_value(<<"path">>, QueryJson, <<>>),
    FileName = wh_json:get_value(<<"file_name">>, QueryJson),
    BucketName = zt_util:to_str(application:get_env(erlcloud, s3_bucket, "unknown")),
    ExpiredDuration = zt_util:to_integer(application:get_env(erlcloud, s3_presigned_expired, 60)),
    {ok, Conf} = erlcloud_aws:profile(),
    Key = zt_util:to_str(<<Path/binary,"/",FileName/binary>>),
    case Path of 
        <<>> -> zt_util:to_str(FileName);
        _ -> zt_util:to_str(<<Path/binary,"/",FileName/binary>>)
    end,
    Url = erlcloud_s3:make_presigned_v4_url(ExpiredDuration,BucketName,Key,[],Conf),
    RespData = #{
        expired_duration => ExpiredDuration,
        url => zt_util:to_bin(Url)
    },
	{Req, 
		cb_context:setters(Context,[
			{fun cb_context:set_resp_data/2, RespData},
			{fun cb_context:set_resp_status/2, 'success'}
	])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			 

validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context
						   ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context,_) ->
	Context.

validate_request(Context, ?PATH_PRESIGNED, ?HTTP_GET) ->
    Context1 = cb_context:setters(Context
						   ,[{fun cb_context:set_resp_status/2, 'success'}]),
    QueryJson = cb_context:query_string(Context),
    Val = wh_json:get_value(<<"file_name">>, QueryJson, <<>>),
    api_util:check_val(Context1, <<"file_name">>, Val);

validate_request(Context,_, _) ->
	Context.