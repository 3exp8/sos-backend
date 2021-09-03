%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API manage tax code
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_apis).

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

-define(APIS, 
	[
		{<<"groups">>,cb_group},
		{<<"users">>,cb_user},
		{<<"auth">>,cb_auth},
		{<<"forgot">>,cb_forgot},
		{<<"reset">>,cb_reset}
	]
).

init() ->
	_ = crossbar_bindings:bind(<<"*.resource_exists.apis">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.apis">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.apis">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.authorize.apis">>, ?MODULE, 'authorize'),
	_ = crossbar_bindings:bind(<<"*.allowed_methods.apis">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.apis">>, ?MODULE, 'handle_get').

-spec allowed_methods() -> http_methods().
allowed_methods() ->
	[?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
	[?HTTP_GET].

-spec resource_exists() -> 'true'.

%% /api/v1/tax_codes/{id}
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.

%% /api/v1/tax_codes/{id}
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
validate(Context,_) ->
	validate_request(Context, cb_context:req_verb(Context)).  

handle_get({Req, Context}) -> 
QueryJson = cb_context:query_string(Context),
  Type = wh_json:get_value(<<"type">>, QueryJson, <<>>),
  case Type of 
	<<"fail">> -> 
			{Req, cb_context:setters(Context, [
				{fun cb_context:set_resp_error_msg/2, <<"Found no Data">>},
				{fun cb_context:set_resp_status/2, <<"error">>},
				{fun cb_context:set_resp_error_code/2, 404}] )};
	<<"success">> -> 
			{Req, cb_context:setters(Context,[{fun cb_context:set_resp_data/2, #{}}
			,{fun cb_context:set_resp_status/2, 'success'}])};
	_ -> 	
			RespData = get_list_errors(),
			lager:debug("RespData ~p~n",[RespData]),
			{Req, 
				cb_context:setters(Context,[
					{fun cb_context:set_resp_data/2, RespData},
					{fun cb_context:set_resp_status/2, 'success'}
			])}
end.

handle_get({Req, Context}, Endpoint) -> 
	RespData = get_list_errors(Endpoint),
	lager:debug("RespData ~p~n",[RespData]),
	{Req, 
		cb_context:setters(Context,[
			{fun cb_context:set_resp_data/2, RespData},
			{fun cb_context:set_resp_status/2, 'success'}
	])}.


get_list_errors(Name)  -> 
	ApiMap = maps:from_list(?APIS),
	case maps:get(Name,ApiMap,<<>>) of 
		<<>> ->  [];
		Module -> list_errors_by_module(Name,Module)
	end.

list_errors_by_module(Name, Module) -> 
	case erlang:function_exported(Module,errors,0) of
		true ->
			Apis = Module:errors(),
			ApisValidates = 
			lists:map(fun( #{
				method := Method,
				path := Path,
				validate_reponses := ValidateResponsesRaw
			}
			) -> 
			ValidateResponses = 
			case ValidateResponsesRaw of 
				{[]} -> #{};
				_ -> ValidateResponsesRaw
			end,
			#{
				method => Method,
				path => Path,
				validate_reponses => #{
					data => ValidateResponses,
					errors_data => api_util:format_error_responses([], ValidateResponsesRaw)
				}
			}

			end,Apis),
			ApisValidates;
		_ ->
			lager:warning("~p:errors() is not defined ~n",[Module]),
			[]
	end.

get_list_errors()  -> 
	Modules = ?APIS,
	lists:foldl(fun({Name,Module}, Acc) -> 
		NewErrorMap = #{
			Name => list_errors_by_module(Name, Module)
		},
		maps:merge(Acc, NewErrorMap)
	end,#{},Modules).

handle_get1({Req, Context}) ->
   Capacity = 2,
   Context1 = api_util:validate_error(Context, <<"max_deals">>, <<"reached">>, <<"You reached max of ",Capacity/integer," deals per day!">>),
   Data = cb_context:validation_errors(Context1),
   lager:debug("Error Data: ~p~n",[Data]),
   RespData = #{
	   data => [Data]
   },
{Req, cb_context:setters(Context,[{fun cb_context:set_resp_data/2, RespData}
          ,{fun cb_context:set_resp_status/2, 'success'}])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			 

validate_request(Context, ?HTTP_GET) ->
	ReqJson = cb_context:req_json(Context),
	QueryJson = cb_context:query_string(Context),
  	Type = wh_json:get_value(<<"type">>, QueryJson, <<>>),
	  case Type of 
	  <<"validate">> -> 
			Context1 = cb_context:setters(Context
					   ,[{fun cb_context:set_resp_status/2, 'success'}]),
					   ValidateFuns = [
						fun validate_max_deals1/2,
						fun validate_max_deals/2
						],
			  lists:foldl(fun(F, Ctx) -> F(ReqJson, Ctx) end, Context1, ValidateFuns);
		_ -> 

		cb_context:setters(Context
						   ,[{fun cb_context:set_resp_status/2, 'success'}])
	end;

validate_request(Context,_) ->
	Context.

validate_max_deals1(ReqJson, Context) -> 
Capacity = 2,
api_util:validate_error(Context, <<"max_deals">>, <<"reached">>, <<"You reached max of 2 deals per day!">>).

validate_max_deals(ReqJson, Context) -> 
	Capacity = 2,
	api_util:validate_error(Context, <<"max_deals3">>, <<"reached">>, <<"You reached max of 3 deals per day!">>).