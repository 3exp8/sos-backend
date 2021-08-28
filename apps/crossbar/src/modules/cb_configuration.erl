-module(cb_configuration).

-include("crossbar.hrl").

-define(PATH_SYSTEM, <<"system">>).

-export([init/0
	,allowed_methods/0
	,allowed_methods/1
	,authorize/1
    ,authorize/2
	,validate/1
	,validate/2
	,resource_exists/0
	,resource_exists/1
	,authenticate/1
	,authenticate/2
	,handle_get/1
	,handle_get/2
	,handle_put/1
	,handle_post/2
	,handle_delete/2
	]).

init() ->
    _ =	crossbar_bindings:bind(<<"*.resource_exists.configurations">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.validate.configurations">>, ?MODULE, validate),
    _ =	crossbar_bindings:bind(<<"*.authenticate.configurations">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.configurations">>, ?MODULE, authorize),
    _ =	crossbar_bindings:bind(<<"*.allowed_methods.configurations">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.to_json.get.configurations">>, ?MODULE, handle_get),
    _ =	crossbar_bindings:bind(<<"*.execute.post.configurations">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.put.configurations">>, ?MODULE, handle_put),
    _ =	crossbar_bindings:bind(<<"*.execute.delete.configurations">>, ?MODULE, handle_delete).

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().


%% /api/v1/configurations
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/configurations/id
allowed_methods(_Path) ->
	[?HTTP_GET,?HTTP_POST,?HTTP_DELETE].


-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.


%% /api/v1/configurations
resource_exists() -> 'true'.
% /api/v1/configurations/id
resource_exists(_Path) -> 'true'.


%% /api/v1/configurations
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) -> 
	Token = cb_context:auth_token(Context),
	app_util:oauth2_authentic(Token, Context).

%% /api/v1/configurations/configurationid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, ?PATH_SYSTEM) -> true;
authenticate(Context, _ConfigurationId) -> 
	Token = cb_context:auth_token(Context),
	app_util:oauth2_authentic(Token, Context).

%% /api/v1/configurations
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
	authorize_util:authorize(?MODULE, Context).

%% /api/v1/configurations/id
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(Context, ?PATH_SYSTEM) ->
    authorize_util:authorize(?MODULE, Context);
authorize(Context, _Id) ->
    authorize_util:authorize(?MODULE, Context).

%% /api/v1/configurations
-spec validate(cb_context:context()) ->  cb_context:context().
validate(_Context) ->
	validate_configuration(_Context, cb_context:req_verb(_Context)).

%% /api/v1/configurations/id
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
validate(_Context, _PATH) ->
    validate_configuration(_Context, _PATH, cb_context:req_verb(_Context)).


-spec validate_configuration(cb_context:context(), http_method()) -> cb_context:context().
-spec validate_configuration(cb_context:context(), path_token(), http_method()) -> cb_context:context().


%% PUT /api/v1/configurations
validate_configuration(Context, ?HTTP_PUT = Verb) ->
	lager:info("validate request: PUT ~n",[]),
	validate_request(Context, Verb);

%% GET /api/v1/configurations
validate_configuration(Context, ?HTTP_GET = Verb) ->
	lager:info("validate request: GET LIST: ~n",[]),
	validate_request(Context, Verb).

%% POST /api/v1/configurations/id
validate_configuration(Context, _Path, ?HTTP_POST = Verb) ->
	lager:info("validate request: POST ~n",[]),
	validate_request(_Path, Context, Verb);


%% POST /api/v1/configurations/id
validate_configuration(Context, _Path, ?HTTP_DELETE = Verb) ->
	lager:info("validate request: DELETE ~n",[]),
	validate_request(_Path, Context, Verb);

%% GET /api/v1/configurations/id
validate_configuration(Context, _Path, ?HTTP_GET = Verb) ->
	lager:info("validate request: GET DETAIL ~n",[]),
	validate_request(_Path, Context, Verb).


-spec handle_get(req_ctx()) -> req_ctx().
-spec handle_get(req_ctx(), path_token()) -> req_ctx().

%% GET api/v1/configurations

handle_get({Req, Context}) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  	Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),

	% Role = cb_context:role(Context),
	% case Role of 
	% 	?USER_ROLE_CUSTOMER ->
	% 		Customer_id = cb_context:customer_id(Context),
	% 		lager:info("Customer ID: ~p~n",[Customer_id]),
	% 		Configurations = configuration_db:find_by_conditions([{customer_id,Customer_id}], PropQueryJson, Limit, Offset);
	% 	?USER_ROLE_USER ->
	% 		Configurations = configuration_db:find_by_conditions([], PropQueryJson, Limit, Offset)
	% end,
	Configurations = configuration_db:find_by_conditions([], PropQueryJson, Limit, Offset),
	lager:debug("Data ~p~n",[Configurations]),
  	ProConfigurations = lists:map(
			fun(Configuration) -> get_sub_fields(Configuration) end, Configurations),
  	{Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, ProConfigurations}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])}.


% GET api/v1/configurations/id

handle_get({Req, Context}, ?PATH_SYSTEM) ->
	QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  	Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	Configurations = configuration_db:find_by_conditions([{account_id,<<>>}], PropQueryJson, Limit, Offset),
	lager:debug("Data ~p~n",[Configurations]),
  	ProConfigurations = lists:map(
			fun(Configuration) -> get_system_sub_fields(Configuration) end, Configurations),
  	{Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, ProConfigurations}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])};

handle_get({Req, Context}, _ConfigurationId) ->
	_Role = cb_context:role(Context),
	Configuration = configuration_db:find(_ConfigurationId),
	case Configuration of 
		notfound ->
			lager:info(<<"NOT FOUND">>),
      		{Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Configuration Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}])};
    	_ -> 
      		PropConfiguration = get_sub_fields(Configuration),
          		{Req, cb_context:setters(Context
                                   ,[{fun cb_context:set_resp_data/2, PropConfiguration}
                                     ,{fun cb_context:set_resp_status/2, 'success'}
                                    ])}    	
  	end.  


%% PUT api/v1/configurations
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
	lager:debug("Handle PUT"),
	ReqJson = cb_context:req_json(Context),
	% Customer_id = cb_context:customer_id(Context),
	Account_id = <<>>,
	ConfigurationDb = get_configuration_data(ReqJson,Account_id),
	Resp = get_sub_fields(ConfigurationDb),
	cb_context:setters(Context
                     ,[{fun cb_context:set_resp_data/2, Resp}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                      ]).

handle_post(Context, Id) ->
	AccountId = cb_context:customer_id(Context),
	% Role = cb_context:role(Context),
	Configuration = configuration_db:find(Id),
	case Configuration of 
		notfound ->
			lager:info(<<"NOT FOUND">>),
      		cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Configuration Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}]);
		_ ->
			ReqJson =  cb_context:req_json(Context),
			ConfigurationInfo = update_configuration(ReqJson,Configuration),
			NewConfiguration = maps:merge(ConfigurationInfo, #{updated_by_id => AccountId}),
			configuration_db:save(NewConfiguration),
			RespData = get_sub_fields(NewConfiguration),
    		cb_context:setters(Context
                  ,[{fun cb_context:set_resp_data/2, RespData}
                    ,{fun cb_context:set_resp_status/2, 'success'}
                   ])	
	end.  
	  
handle_delete(Context, Id) ->
	ConfigurationInfo = configuration_db:find(Id),
	lager:debug("~n Find: ~p",[ConfigurationInfo]),
	case ConfigurationInfo of 
		notfound ->
      		cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Configuration Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}]);
    	_ -> 
			Success = configuration_db:del_by_id(Id),
			lager:debug("~n state ~p",[Success]),
				RespData =#{
					updated_time_dt => zt_datetime:get_now(),
					updated_by => <<"account123456">>
				},
				cb_context:setters(Context
                    ,[{fun cb_context:set_resp_data/2, RespData}
                     ,{fun cb_context:set_resp_status/2, 'success'}
					])
		
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_configuration(ReqJson, Configuration) ->
	Type  = maps:get(type,Configuration,<<>>),
	Group = maps:get(group,Configuration,<<>>),
	Key   = maps:get(key,Configuration,<<>>),
	Value = wh_json:get_value(<<"value">>,ReqJson,maps:get(value,Configuration,<<>>)),
	Content_type = wh_json:get_value(<<"content_type">>,ReqJson,maps:get(content_type,Configuration,<<>>)),
	UpdateTime =  zt_datetime:get_now(),	
	maps:merge(Configuration,
			#{
				type => Type,
				group => Group,
				key => Key,
				value => Value,
				content_type => Content_type,
				updated_time_dt => UpdateTime
			}).

get_configuration_data(ReqJson, AccountId) ->
	CreatedTime = zt_datetime:get_now(),
			ConfigurationInfo = get_configuration_info(ReqJson),
			Uuid = zt_util:get_uuid(),
			ConfigurationId = <<"configuration",Uuid/binary>>,
			ConfigurationDb = maps:merge(ConfigurationInfo, #{
				id => ConfigurationId, 
				account_id => AccountId,
				created_by => AccountId,                               
				updated_by => AccountId,
				updated_time_dt => CreatedTime,
				created_time_dt => CreatedTime
			}),
	configuration_db:save(ConfigurationDb),
	ConfigurationDb.

get_configuration_info([]) -> #{};

get_configuration_info(ReqJson) ->
	Group = wh_json:get_value(<<"group">>,ReqJson,<<>>),
	Key = wh_json:get_value(<<"key">>,ReqJson,<<>>),
	Value = wh_json:get_value(<<"value">>,ReqJson,<<>>),
	ValueObj = wh_json:get_value(<<"value_obj">>,ReqJson,[]),
	ContentType =wh_json:get_value(<<"content_type">>,ReqJson,<<"string">>),
	Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
	#{
		name => wh_json:get_value(<<"name">>,ReqJson,<<>>),
		group => Group,
		key => Key,
		value => Value,
		value_obj => maps:from_list(ValueObj),
	  	content_type => ContentType,
        type => Type
	}.

-spec  validate_request(cb_context:context(), http_method()) -> cb_context:context().
validate_request(Context, ?HTTP_PUT) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [
					fun validate_type/2,
					fun validate_group/2
	],
	lists:foldl(fun(F, C) ->
					F(ReqJson, C)
				end, Context1,  ValidateFuns);
validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context,
			   [{fun cb_context:set_resp_status/2, success}]);
validate_request(Context, _Verb) ->
	cb_context:setters(Context
                        ,[{fun cb_context:set_resp_status/2, 'success'}]).

%%GET api/v1/configurations/id
-spec  validate_request(path_token(), cb_context:context(), http_method()) -> cb_context:context().
validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context,
			   [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, ?HTTP_POST) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
                                ,[{fun cb_context:set_resp_status/2, 'success'}]),	 
	ValidateFuns = [fun validate_value/2, fun validate_content_type/2],
	lists:foldl(fun(F, C) ->
					F(ReqJson, C)
				end, Context1,  ValidateFuns);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context,
			   [{fun cb_context:set_resp_status/2, success}]);
			   
validate_request(_Id, Context, _Verb) -> Context.


is_key_exist( Key) ->
	case configuration_db:find_by_conditions([{key,Key}], [], 1, 0) of 
    [ConfigurationDb] when is_map(ConfigurationDb) ->
      true ;
    [] ->
      false ;
    Error ->
      lager:error("Configuration Can't Register. Maybe Database With This Key: ~p; Error: ~p ~n", [Key,Error]),
      throw(dberror)
  end.

-spec validate_type(api_binary(),cb_context:context())->cb_context:cb_context().
validate_type(ReqJson, Context) ->
	Type = wh_json:get_value(<<"type">>,ReqJson,<<>>),
  	case Type of
		<<>> ->
			  lager:debug("Required TYPE"),
			  api_util:validate_error(Context,<<"type">>,<<"required">>,<<"Field 'type' is required">>);
		_ -> Context
	end.

-spec validate_value(api_binary(),cb_context:context())->cb_context:cb_context().
validate_value(ReqJson, Context) ->
	Value = wh_json:get_value(<<"value">>,ReqJson,<<>>),
	case Value of 
		<<>> ->
				lager:debug("Required Value"),
				api_util:validate_error(Context,<<"value">>,<<"required">>,<<"Field 'value' is required">>);
		_ -> Context
	end.

-spec validate_group(api_binary(),cb_context:context()) -> cb_context:cb_context().
validate_group(ReqJson, Context) ->
	Group = wh_json:get_value(<<"group">>,ReqJson,<<>>),
	case Group of 
		<<>> -> 
			lager:debug("Required Group"),
			api_util:validate_error(Context,<<"group">>,<<"required">>,<<"Field 'group' is required">>);
		_ -> Context
	end.

-spec validate_content_type(api_binary(),cb_context:context())->cb_context:cb_context().
validate_content_type(ReqJson, Context) ->
	Value = wh_json:get_value(<<"content_type">>,ReqJson,<<>>),
	case Value of 
		<<>> ->
				lager:debug("Required Content Type"),
				api_util:validate_error(Context,<<"content_type">>,<<"required">>,<<"Field 'content_type' is required">>);
		_ -> Context
	end.

get_sub_fields(Configuration) ->
  Fields = [created_by, created_time_dt, updated_by, updated_time_dt, account_id] ,
  NewMap = maps:without(Fields, Configuration),
  Res = maps:to_list(NewMap),
  proplists:substitute_aliases([], Res).

get_system_sub_fields(Configuration) ->
	Fields = [account_id, created_by, created_time_dt, updated_by, updated_time_dt] ,
	NewMap = maps:without(Fields, Configuration),
	Res = maps:to_list(NewMap),
	proplists:substitute_aliases([], Res).