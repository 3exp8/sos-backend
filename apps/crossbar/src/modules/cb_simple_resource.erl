-module(cb_simple_resource).

-include("crossbar.hrl").

-export([init/0]).
-export([allowed_methods/0, content_types_provided/1, authenticate/1]).
-export([handle_get/1]).

init() ->
	crossbar_bindings:bind(<<"*.allowed_methods.resource">>, ?MODULE, 'allowed_methods'),
    crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize.resource">>, ?MODULE, 'authorize'),
    crossbar_bindings:bind(<<"*.content_types_provided.resource">>, ?MODULE, 'content_types_provided'),
     crossbar_bindings:bind(<<"*.to_json.get.resource">>, ?MODULE, 'handle_get').


allowed_methods() ->
	[<<"GET">>, <<"POST">>].
	
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
	Token = cb_context:auth_token(Context),
	lager:info("Token: ~p ~n",[Token]),
	case oauth2:verify_access_token(Token, []) of
                {ok, _Identity} ->
                    true;
                _  ->
                    false
    end.
      
-spec content_types_provided(cb_context:context()) -> cb_context:context().
 content_types_provided(Context) ->
    CTPs = [{'to_json', [{<<"application">>, <<"json">>}]}],
    cb_context:add_content_types_provided(Context, CTPs).

handle_get({Req, Context}) ->
	Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]),	
	{Req, Context1}.