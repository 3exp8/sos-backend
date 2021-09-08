-module(tel4vn_voice_otp).

-export([call/2, start_conn/2]).

-define(Protocol, application:get_env(zt_r2r, reminder_protocol, https)).
-define(Host, application:get_env(zt_r2r, reminder_host, "reminder-api")).
-define(Port, application:get_env(zt_r2r, reminder_port, 80)).
-define(Base64Secret, zt_util:to_bin(application:get_env(zt_r2r, reminder_secret_key, ""))).
-define(CampaignId, zt_util:to_bin(application:get_env(zt_r2r, reminder_campaign_id, ""))).
-define(Caller, zt_util:to_bin(application:get_env(zt_r2r, caller, ""))).

-define(REMINDER_ACCESS_TOKEN_API, "/api/v1/users/generate_access_token").
-define(REMINDER_CALL_START_API, "/api/v2/campaigns/").
-define(REMINDER_CALL_END_API, "/otp").
-define(TOKEN_KEY, sostokenkey).


start_conn(Host, Port)->
	start_conn(http, Host, Port).

start_conn(http, Host, Port)->
	{ok, Conn} = shotgun:open(Host, Port),
	Conn;

start_conn(https, Host, Port)->
	{ok, Conn} = shotgun:open(Host, Port, https),
	Conn.

stop_conn(Conn)->
	shotgun:close(Conn),
	ok.

call(To, CallInfo) ->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Token = get_tel4vn_token(),
	{ok, Resp} = call(Conn, Token, To, CallInfo),
	lager:info("AutoCall Response: ~p ~n", [Resp]),
	stop_conn(Conn).

call(Conn, Token, To, CallInfo) when is_binary(To) ->
	{Header, Body} = get_call_request(Token, ?Caller, To, CallInfo),
	Path = buildPath(),
	shotgun:post(Conn, Path, Header, Body, #{});

call(Conn, Token, To, CallInfo) when is_list(To) ->
	{Header, Body} = get_call_request(Token, ?Caller, To, CallInfo),
	Path = buildPath(),
	shotgun:post(Conn, Path, Header, Body, #{}).

get_reminder_token() ->
	Conn = start_conn(?Protocol, ?Host, ?Port),
	Path = ?REMINDER_ACCESS_TOKEN_API,
	{Header, Body} = get_call_access_token_request(?Base64Secret),
	{ok, Response} = shotgun:post(Conn, Path, Header, Body, #{}),
	Token = get_call_access_token_response_value(Response),
	lager:info("AutoCall Token: ~p ~n", [Token]),
	stop_conn(Conn),
	Token.

buildPath() ->
	Path = ?REMINDER_CALL_START_API ++ binary_to_list(?CampaignId) ++ ?REMINDER_CALL_END_API,
	Path.

get_tel4vn_token() ->
	sos_cache:init(),
	case sos_cache:get(?TOKEN_KEY) of
		{ok, Token} ->
			Token;
		{error, not_found} ->
			Token = get_reminder_token(),
			sos_cache:set(?TOKEN_KEY, Token),
			Token
	end.

get_call_access_token_request(SecretKey) ->
	Header = 	[
		{<<"Accept">>,<<"application/json">>},
		{<<"Content-Type">>,<<"application/json">>}
	],
	PrePayload = #{
		secret_key => SecretKey
	},
	Payload = maps:to_list(PrePayload),
	Body = jsx:encode(Payload),
	{Header, Body}.

get_call_access_token_response_value(Response) ->
	ResBody = maps:get(body,Response),
	ResDecode = jsx:decode(ResBody),
	Content = proplists:get_value(<<"data">>, ResDecode),
	ContentMap = maps:from_list(Content),
	Token = maps:get(<<"access_token">>, ContentMap),
	Token.

get_call_request(Token, Caller, To, CallInfo) ->
	Header =[
		{<<"Accept">>,<<"application/json">>},
		{<<"Content-Type">>,<<"application/json">>},
		{<<"Authorization">>,Token}
	],
	PrePayload = #{
		caller => Caller,
		callees => [To],
		params => #{
			template_name => 'covidai_template_otp_1',
			code_otp => CallInfo
		}
	},
	Payload = maps:to_list(PrePayload),
	Body = jsx:encode(Payload),
	{Header, Body}.
