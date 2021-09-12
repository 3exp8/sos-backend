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
	authenticate_verb(Context, cb_context:req_verb(Context)).

authenticate_verb(_Context, ?HTTP_GET)  -> true;

authenticate_verb(Context, _) ->
		Token = cb_context:auth_token(Context),
		app_util:oauth2_authentic(Token, Context).

%% /api/v1/configurations/configurationid
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, ?PATH_SYSTEM) -> true;

authenticate(Context, _ConfigurationId) -> 
	authenticate_verb(Context, cb_context:req_verb(Context)).

%% /api/v1/configurations
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
	authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(_Context, ?HTTP_GET)  -> true;

authorize_verb(Context, _) ->
	Role = cb_context:role(Context),
	Role == ?USER_ROLE_ADMIN.

%% /api/v1/configurations/id
-spec authorize(cb_context:context(), path_token()) -> boolean().

authorize(Context, ?PATH_SYSTEM) ->
	authorize_verb(Context, cb_context:req_verb(Context));

authorize(Context, _Id) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

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
	Configurations = configuration_db:find_by_conditions([], PropQueryJson, Limit, Offset),
  	ProConfigurations = 
	  lists:map(
			fun(Configuration) -> get_sub_fields(Configuration) 
	  end, Configurations),
  	{Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, ProConfigurations}
                                  ,{fun cb_context:set_resp_status/2, 'success'}
							])}.


% GET api/v1/configurations/id

handle_get({Req, Context}, ?PATH_SYSTEM) ->
	QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
  	Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	Configurations = configuration_db:find_by_conditions([{account_id,<<>>}], PropQueryJson, Limit, Offset),
  	ProConfigurations = lists:map(
			fun(Configuration) -> get_system_sub_fields(Configuration) 
	end, Configurations),
  	{Req, cb_context:setters(Context
                                ,[{fun cb_context:set_resp_data/2, ProConfigurations}
                                  ,{fun cb_context:set_resp_status/2, 'success'}])};

handle_get({Req, Context}, Id) ->
	Configuration = configuration_db:find(Id),
	case Configuration of 
		notfound ->
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
	ReqJson = cb_context:req_json(Context),
	UserId = cb_context:user_id(Context),
	case create_configuration_data(ReqJson,UserId) of 
	{error, Error} -> 
			cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, Error},
			{fun cb_context:set_resp_status/2, <<"error">>},
			{fun cb_context:set_resp_error_code/2, 400}]);
	ConfigurationDb -> 
		Resp = get_sub_fields(ConfigurationDb),
		cb_context:setters(Context
						,[{fun cb_context:set_resp_data/2, Resp}
						,{fun cb_context:set_resp_status/2, 'success'}
						])
	end.

handle_post(Context, Id) ->
	UserId = cb_context:user_id(Context),
	case configuration_db:find(Id) of 
		notfound ->
      		cb_context:setters(Context, [
				  				{fun cb_context:set_resp_error_msg/2, <<"Configuration Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}
							]);
		#{
			value := ValueDb,
			content_type := ContentTypeDb
		} = InfoDb ->
			ReqJson =  cb_context:req_json(Context),
			Value = wh_json:get_value(<<"value">>,ReqJson,ValueDb),
			ContentType = wh_json:get_value(<<"content_type">>,ReqJson,ContentTypeDb),
			NewInfo = 
				maps:merge(InfoDb,#{
							value => Value,
							content_type => ContentType,
							updated_by_id => UserId,
							updated_time_dt => zt_datetime:get_now()
				}),
			configuration_db:save(NewInfo),
			RespData = get_sub_fields(NewInfo),
    		cb_context:setters(Context
                  ,[{fun cb_context:set_resp_data/2, RespData}
                    ,{fun cb_context:set_resp_status/2, 'success'}
                   ])	
	end.  
	  
handle_delete(Context, Id) ->
	case configuration_db:find(Id) of 
		notfound ->
      		cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Configuration Not Found">>},
                                {fun cb_context:set_resp_status/2, <<"error">>},
                                {fun cb_context:set_resp_error_code/2, 404}]);
    	_ -> 
			configuration_db:del_by_id(Id),
			RespData = #{
					status => <<"success">>,
					id => Id
			},
			cb_context:setters(Context
                    ,[{fun cb_context:set_resp_data/2, RespData}
                     ,{fun cb_context:set_resp_status/2, 'success'}
			])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_configuration_data(ReqJson, UserId) ->
	Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
	Group = wh_json:get_value(<<"group">>,ReqJson,<<>>),
	Key = wh_json:get_value(<<"key">>,ReqJson,<<>>),

	case configuration_handler:is_key_exist(Type, Group, Key) of 
		false -> 
			ConfigurationInfo = get_configuration_info(ReqJson),
			Uuid = zt_util:get_uuid(),
			ConfigurationDb = 
				maps:merge(ConfigurationInfo, #{
					id => <<"configuration",Uuid/binary>>, 
					created_by => UserId,                               
					created_time_dt => zt_datetime:get_now()
			}),
			configuration_db:save(ConfigurationDb);
		OtherData ->
			lager:error("error when create conf Type: ~p, Group: ~p, Key: ~p, OtherData: ~p ~n",[Type, Group, Key,OtherData]), 
			{error, existed}
	end.

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
					fun configuration_handler:validate_type/2,
					fun configuration_handler:validate_group/2
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
	ValidateFuns = [
		fun configuration_handler:validate_value/2, 
		fun configuration_handler:validate_content_type/2
	],
	lists:foldl(fun(F, C) ->
					F(ReqJson, C)
				end, Context1,  ValidateFuns);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context,
			   [{fun cb_context:set_resp_status/2, success}]);
			   
validate_request(_Id, Context, _Verb) -> Context.

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