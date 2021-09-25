%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API manage tax code
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_systemctl).

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
	,handle_post/2
	,handle_get/2

	]).

-define(PATH_CLUSTER,<<"cluster">>).
-define(PATH_INITDATA,<<"initdata">>).
-define(PATH_INFO,<<"info">>).

init() ->
	_ = crossbar_bindings:bind(<<"*.resource_exists.systemctl">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.systemctl">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.systemctl">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.authorize.systemctl">>, ?MODULE, 'authorize'),
	_ = crossbar_bindings:bind(<<"*.allowed_methods.systemctl">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.systemctl">>, ?MODULE, 'handle_get'),
    _ = crossbar_bindings:bind(<<"*.execute.post.systemctl">>, ?MODULE, 'handle_post').


-spec allowed_methods() -> http_methods().
allowed_methods() -> [].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
	[?HTTP_POST,?HTTP_GET].

-spec resource_exists() -> 'true'.

%% /api/v1/s3/{id}
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.

%% /api/v1/s3/{id}
resource_exists(_) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(_Context) ->  true.

-spec authenticate(cb_context:context(),path_token()) -> boolean().
authenticate(_Context,?PATH_INFO) ->  true;

authenticate(Context,?PATH_CLUSTER) -> 
	Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context);

authenticate(_Context,?PATH_INITDATA) ->  true.

-spec authorize(cb_context:context()) -> boolean().
authorize(_Context) -> true.

-spec authorize(cb_context:context(),path_token()) -> boolean().
authorize(Context,?PATH_CLUSTER) -> 
	Role = cb_context:role(Context),
	Role == ?USER_ROLE_SYSTEM;

authorize(_Context,?PATH_INITDATA) -> true;
authorize(_Context,?PATH_INFO) -> true.

-spec validate(cb_context:context() ) ->  cb_context:context().

%% Validate resource : /api/v1/tax_codes/{id}
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).   

-spec validate(cb_context:context(),path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/tax_codes/{id}
validate(Context,Path) ->
	validate_request(Context, Path, cb_context:req_verb(Context)).  

-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, ?PATH_CLUSTER) ->
    ReqJson =  cb_context:req_json(Context),
    Action = wh_json:get_value(<<"action">>,ReqJson),
    case systemctl_handler:handle_cluster(Action,ReqJson) of 
	{error, ErrData} -> 
		cb_context:setters(Context,[
			{fun cb_context:set_resp_error_msg/2, ErrData},
			{fun cb_context:set_resp_status/2, <<"error">>},
			{fun cb_context:set_resp_error_code/2, 503}
		]);
	_ -> 
		RespData  = #{
			status => <<Action/binary," success">>,
			nodes => systemctl_handler:list_nodes()
		},
		cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
									,{fun cb_context:set_resp_status/2, 'success'}])
	end;

handle_post(Context, ?PATH_INITDATA) ->
  ReqJson =  cb_context:req_json(Context),
  DataType = wh_json:get_value(<<"data_type">>,ReqJson),
  Data = wh_json:get_value(<<"data">>, ReqJson),
  lager:debug("Data: ~p~n",[Data]),
  case systemctl_handler:handle_init_data(DataType, {Data}) of 
  {error, ErrData} -> 
		cb_context:setters(Context,[
			{fun cb_context:set_resp_error_msg/2, ErrData},
			{fun cb_context:set_resp_status/2, <<"error">>},
			{fun cb_context:set_resp_error_code/2, 400}
		]);
  SuccessData -> 
	cb_context:setters(Context, [{fun cb_context:set_resp_data/2, SuccessData}
										,{fun cb_context:set_resp_status/2, 'success'}])
end.

handle_get({Req, Context}, ?PATH_INFO) ->
    RespData  = #{
		node => node(),
		cluster => systemctl_handler:list_nodes()
	},
	{Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, RespData}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])};

handle_get({Req, Context}, ?PATH_CLUSTER) -> 
    RespData  = #{
        nodes => systemctl_handler:list_nodes()
    },
	{Req, 
		cb_context:setters(Context,[
			{fun cb_context:set_resp_data/2, RespData},
			{fun cb_context:set_resp_status/2, 'success'}
	])};

handle_get({Req, Context}, ?PATH_INITDATA) -> 
    RespData = <<"TODO">>,
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
		,[{fun cb_context:set_resp_status/2, 'success'}]).

validate_request(Context, ?PATH_CLUSTER, ?HTTP_GET) ->
	cb_context:setters(Context
		,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?PATH_INFO, ?HTTP_GET) ->
        cb_context:setters(Context
            ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?PATH_INITDATA, ?HTTP_GET) ->
        cb_context:setters(Context
            ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?PATH_CLUSTER, ?HTTP_POST) ->
    
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
            fun systemctl_handler:validate_cluster_action/2,
            fun systemctl_handler:validate_cluster_node/2

    ],
    lists:foldl(fun (F, C) ->
            F(ReqJson, C)
        end,Context1,ValidateFuns);

validate_request(Context, ?PATH_INITDATA, ?HTTP_POST) ->
    
		ReqJson = cb_context:req_json(Context),
		Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
		ValidateFuns = [
				fun systemctl_handler:validate_initdata_data_type/2,
				fun systemctl_handler:validate_initdata_data/2	
		],
		lists:foldl(fun (F, C) ->
				F(ReqJson, C)
			end,Context1,ValidateFuns);

validate_request(Context,_, _) ->
	Context.