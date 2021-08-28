%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
%%% @doc
%%% Moved util functions out of v1_resource so only REST-related calls
%%% are in there.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Jon Blantonis_authentic
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------
-module(api_util).

-include("crossbar.hrl").


-export([is_cors_preflight/1
				 ,is_cors_request/1
				 ,add_cors_headers/2
				 ,allow_methods/3
				 ,parse_path_tokens/2
				 ,get_req_data/2
				 ,get_http_verb/2
				 ,is_authentic/2
				 ,is_permitted/2
				 ,is_known_content_type/2
				 ,does_resource_exist/1
				 ,validate/1
				 ,succeeded/1
				 ,execute_request/2
				 ,finish_request/2
				 ,create_push_response/2
				 ,set_resp_headers/2
				 ,create_resp_content/2
				 ,create_pull_response/2
				 ,halt/2
				 ,content_type_matches/2
				 ,ensure_content_type/1
				 ,create_event_name/2
				 ,validate_error/4
				 ,validate_error/6
				 ,check_val/3
				 ,check_number_val/3
				 ,check_vals/3
				 ,check_vals/4
				 ,check_val_one_of/4
				 ,check_arr_val/3
				 ,uppercase_all/1
				 ,trans_props/1
				 ,add_error_response/3
				 ,add_error_response/5
				 ,format_error_responses/2
				 ]).

-define(MAX_UPLOAD_SIZE, whapps_config:get_integer(?CONFIG_CAT, <<"max_upload_size">>, 8000000)).

-type halt_return() :: {'halt', cowboy_req:req(), cb_context:context()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource preflight
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_preflight(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
is_cors_preflight(Req0) ->
	case is_cors_request(Req0) of
		{'true', Req1} ->
			case cowboy_req:method(Req1) of
				?HTTP_OPTIONS -> {'true', Req1};
				_M -> {'false', Req1}
			end;
		Nope -> Nope
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if this is a cross origin resource sharing
%% request
%% @end
%%--------------------------------------------------------------------
-spec is_cors_request(cowboy_req:req()) -> {boolean(), cowboy_req:req()}.
-spec is_cors_request(cowboy_req:req(), ne_binaries()) -> {boolean(), cowboy_req:req()}.
is_cors_request(Req) ->
	ReqHdrs = [<<"origin">>, <<"access-control-request-method">>, <<"access-control-request-headers">>],

	is_cors_request(Req, ReqHdrs).
is_cors_request(Req, []) -> {'false', Req};
is_cors_request(Req, [ReqHdr|ReqHdrs]) ->
	Hdr = cowboy_req:header(ReqHdr, Req),
	case Hdr of
		'undefined' -> is_cors_request(Req, ReqHdrs);
		_H -> {'true', Req}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_cors_headers(cowboy_req:req(), cb_context:context()) ->
	cowboy_req:req().
add_cors_headers(Req, Context) ->
	ReqMethod = cowboy_req:header(<<"access-control-request-method">>, Req),

	Methods = [?HTTP_OPTIONS | cb_context:allow_methods(Context)],
	Allow = case wh_util:is_empty(ReqMethod)
							 orelse lists:member(ReqMethod, Methods)
					of
						'false' -> [ReqMethod|Methods];
						'true' -> Methods
					end,

	lists:foldl(fun({H, V}, ReqAcc) ->
									cowboy_req:set_resp_header(H, V, ReqAcc)
							end
							,Req
							,get_cors_headers(Allow)
						 ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_cors_headers(ne_binaries()) -> wh_proplist().
get_cors_headers(Allow) ->
	[{<<"access-control-allow-origin">>, <<"*">>}
	 ,{<<"access-control-allow-methods">>, wh_util:join_binary(Allow, <<", ">>)}
	 ,{<<"access-control-allow-headers">>, <<"Authorization, Content-Type, Depth, User-Agent, X-Http-Method-Override, X-File-Size, X-Requested-With, If-Modified-Since, X-File-Name, Cache-control, X-Auth-Token, If-Match">>}
	 ,{<<"access-control-expose-headers">>, <<"Content-Type, X-Auth-Token, X-Request-ID, Location, Etag, ETag">>}
	 ,{<<"access-control-max-age">>, wh_util:to_binary(?SECONDS_IN_DAY)}
	].

-spec get_req_data(cb_context:context(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
-spec get_req_data(cb_context:context(), {content_type(), cowboy_req:req()}, wh_json:object()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
get_req_data(Context, Req0) ->
	{QS, Req1} = get_query_string_data(Req0),
	get_req_data(Context, get_content_type(Req1), QS).

get_query_string_data(Req0) ->
	%QS0 = cowboy_req:qs(Req0),
	QS0 = cowboy_req:parse_qs(Req0),
	get_query_string_data(QS0, Req0).
get_query_string_data(<<>>, Req) ->
	{wh_json:new(), Req};
get_query_string_data([], Req) ->
	{wh_json:new(), Req};
get_query_string_data(LQS0, Req) when is_binary(LQS0) ->
	%LQS0 = parse_qs(QS0),
	get_query_string_data(LQS0, Req);
get_query_string_data(QS0, Req) ->
	QS = wh_json:from_list(QS0),
	lager:info("get_query_string_data: ~p",[QS]),
	{QS, Req}.

% parse_qs(QS0) ->
% 	case binary:split(QS0, [<<"&">>], [global]) of 
% 	Params when is_list(Params) -> 
% 		Fun = fun(QSEl) ->
% 			case binary:split(QSEl, [<<"=">>], [global]) of 
% 			[Key, Value] -> 

% 				{Key, Value};
% 			_ -> QSEl
% 			end
% 		end,
% 		lists:map(Fun, Params);
% 	_ -> QS0 
% 	end. 
	
-spec get_content_type(cowboy_req:req()) ->
	{api_binary(), cowboy_req:req()}.
get_content_type(Req) ->
	get_parsed_content_type(cowboy_req:parse_header(<<"content-type">>, Req), Req).

-spec get_parsed_content_type( 'undefined' | cowboy_req:req(), cowboy_req:req()) ->
	{api_binary(), cowboy_req:req()}.
get_parsed_content_type(undefined, Req) ->
	{'undefined', Req};
get_parsed_content_type({Main, Sub, _Opts}, Req) ->
	{<<Main/binary, "/", Sub/binary>>, Req}.

get_req_data(Context, {'undefined', Req0}, QS) ->
	{JSON, Req1} = get_json_body(Req0),

	Setters = [{fun cb_context:set_req_json/2, JSON}
						 ,{fun cb_context:set_req_data/2, wh_json:get_value(<<"data">>, JSON, wh_json:new())}
						 ,{fun cb_context:set_query_string/2, QS}
						],
	{lists:foldl(fun({F, D}, C) -> F(C, D) end, Context, Setters)
	 ,Req1
	};
get_req_data(Context, {<<"multipart/form-data">>, Req}, QS) ->
	maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);

%% cURL defaults to this content-type, so check it for JSON if parsing fails
get_req_data(Context, {<<"application/x-www-form-urlencoded">>, Req1}, QS) ->
	maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req1, QS);

get_req_data(Context, {<<"application/json">>, Req1}, QS) ->
	{JSON, Req2} = get_json_body(Req1),

	Setters = [{fun cb_context:set_req_json/2, JSON}
						 ,{fun cb_context:set_req_data/2, wh_json:get_value(<<"data">>, JSON, wh_json:new())}
						 ,{fun cb_context:set_query_string/2, QS}
						],
	{lists:foldl(fun({F, D}, C) -> F(C, D) end, Context, Setters)
	 ,Req2
	};
get_req_data(Context, {<<"application/x-json">>, Req1}, QS) ->
	{JSON, Req2} = get_json_body(Req1),
	Setters = [{fun cb_context:set_req_json/2, JSON}
						 ,{fun cb_context:set_req_data/2, wh_json:get_value(<<"data">>, JSON, wh_json:new())}
						 ,{fun cb_context:set_query_string/2, QS}
						],
	{lists:foldl(fun({F, D}, C) -> F(C, D) end, Context, Setters)
	 ,Req2
	};

get_req_data(Context, {<<"application/base64">>, Req1}, QS) ->
	decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, {<<"application/x-base64">>, Req1}, QS) ->
	decode_base64(cb_context:set_query_string(Context, QS), <<"application/base64">>, Req1);
get_req_data(Context, {<<"multipart/", _C/binary>>, Req}, QS) ->
	maybe_extract_multipart(cb_context:set_query_string(Context, QS), Req, QS);
get_req_data(Context, {ContentType, Req1}, QS) ->
	extract_file(cb_context:set_query_string(Context, QS), ContentType, Req1).

-spec maybe_extract_multipart(cb_context:context(), cowboy_req:req(), wh_json:object()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
maybe_extract_multipart(Context, Req0, QS) ->
	try extract_multipart(Context, Req0, QS) of
		Resp -> Resp
	catch
		_E:_R ->
			handle_failed_multipart(Context, Req0, QS)
	end.

-spec handle_failed_multipart(cb_context:context(), cowboy_req:req(), wh_json:object()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
handle_failed_multipart(Context, Req0, QS) ->
	{ReqBody, Req1} = get_request_body(Req0),

	try get_url_encoded_body(ReqBody) of
		JObj ->
			handle_url_encoded_body(Context, Req1, QS, JObj)
	catch
		_E:_R ->
			try_json(ReqBody, QS, Context, Req1)
	end.

-spec handle_url_encoded_body(cb_context:context(), cowboy_req:req(), wh_json:object(), wh_json:object()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
handle_url_encoded_body(Context, Req, QS, JObj) ->
	case wh_json:get_values(JObj) of
		{['true'], [JSON]} ->
			try_json(JSON, QS, Context, Req);
		_Vs ->
			Setters = [{fun cb_context:set_req_json/2, JObj}
								 ,{fun cb_context:set_req_data/2, wh_json:get_value(<<"data">>, JObj, wh_json:new())}
								 ,{fun cb_context:set_query_string/2, QS}
								],
			{lists:foldl(fun({F, D}, C) -> F(C, D) end, Context, Setters)
			 ,Req
			}
	end.

-spec try_json(ne_binary(), wh_json:object(), cb_context:context(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
try_json(ReqBody, QS, Context, Req) ->
	try get_json_body(ReqBody, Req) of
		{JObj, Req1} ->

			Setters = [{fun cb_context:set_req_json/2, JObj}
								 ,{fun cb_context:set_req_data/2, wh_json:get_value(<<"data">>, JObj, wh_json:new())}
								 ,{fun cb_context:set_query_string/2, QS}
								],
			{lists:foldl(fun({F, D}, C) -> F(C, D) end, Context, Setters)
			 ,Req1
			}
	catch
		'throw':_R ->
			?MODULE:halt(Req, Context);
		_:_ ->
			?MODULE:halt(Req, Context)
	end.

-spec get_url_encoded_body(ne_binary()) -> wh_json:object().
get_url_encoded_body(ReqBody) ->
	wh_json:from_list(cow_qs:parse_qs(ReqBody)).

-type cowboy_multipart_response() :: {'ok', cow_multipart:headers(), cowboy_req:req()} | {'done', cowboy_req:req()} | cowboy_req:req().


-spec extract_multipart(cb_context:context(), cowboy_multipart_response(), wh_json:object()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
extract_multipart(Context, {'done', Req}, _QS) ->
	{Context, Req};
extract_multipart(Context, {'ok', Headers, Req}, QS) ->
	{Ctx, R} = get_req_data(Context, {props:get_value(<<"content-type">>, Headers), Req}, QS),
	extract_multipart(
		Ctx
		,cowboy_req:read_part(R)
		,QS
	 );
extract_multipart(Context, Req, QS) ->
	extract_multipart(
		Context
		,cowboy_req:read_part(Req)
		,QS
	 ).

-spec extract_file(cb_context:context(), ne_binary(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
extract_file(Context, ContentType, Req0) ->
	try extract_file_part_body(Context, ContentType, Req0) of
		Return -> Return
	catch
		_E:_R ->
			extract_file_body(Context, ContentType, Req0)
	end.

-spec extract_file_part_body(cb_context:context(), ne_binary(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
extract_file_part_body(Context, ContentType, Req0) ->
	case cowboy_req:read_part_body(Req0, #{length => ?MAX_UPLOAD_SIZE}) of
		{'more', _, Req1} ->
			handle_max_filesize_exceeded(Context, Req1);
		{'ok', FileContents, Req1} ->
			handle_file_contents(Context, ContentType, Req1, FileContents)
	end.

-spec extract_file_body(cb_context:context(), ne_binary(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
extract_file_body(Context, ContentType, Req0) ->
	case cowboy_req:read_body(Req0, #{length => ?MAX_UPLOAD_SIZE}) of
		{'more', _, Req1} ->
			handle_max_filesize_exceeded(Context, Req1);
		{'ok', FileContents, Req1} ->
			handle_file_contents(Context, ContentType, Req1, FileContents)
	end.

-spec handle_max_filesize_exceeded(cb_context:context(), cowboy_req:req()) ->
	halt_return().
handle_max_filesize_exceeded(Context, Req1) ->
	Maximum = ?MAX_UPLOAD_SIZE,
	MaxLen = wh_util:to_binary(Maximum),
	lager:error("file size exceeded, max is ~p", [Maximum]),
	?MODULE:halt(Req1
							 ,cb_context:add_validation_error(<<"file">>
																								,<<"maxLength">>
																								,wh_json:from_list(
																									 [{<<"message">>, <<"String must not be more than ", MaxLen/binary, " characters">>}
																										,{<<"target">>, Maximum}
																									 ])
																								,Context
																							 )
							).

-spec handle_file_contents(cb_context:context(), ne_binary(), cowboy_req:req(), binary()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
handle_file_contents(Context, ContentType, Req, FileContents) ->
	%% http://tools.ietf.org/html/rfc2045#page-17
	case cowboy_req:header(<<"content-transfer-encoding">>, Req) of
		<<"base64">> ->
			decode_base64(Context, ContentType, Req);
		_Else ->
			ContentLength = cowboy_req:header(<<"content-length">>, Req),
			Headers = wh_json:from_list([{<<"content_type">>, ContentType}
																	 ,{<<"content_length">>, ContentLength}
																	]),
			FileJObj = wh_json:from_list([{<<"headers">>, Headers}
																		,{<<"contents">>, FileContents}
																	 ]),

			Filename = uploaded_filename(Context),
			{cb_context:set_req_files(Context, [{Filename, FileJObj}]), Req}
	end.

-spec uploaded_filename(cb_context:context()) -> ne_binary().
uploaded_filename(Context) ->
	case cb_context:req_value(Context, <<"filename">>) of
		'undefined' -> default_filename();
		Filename ->
			Filename
	end.

default_filename() ->
	<<"uploaded_file_", (wh_util:to_binary(wh_util:current_tstamp()))/binary>>.

-spec decode_base64(cb_context:context(), ne_binary(), cowboy_req:req()) ->
	{cb_context:context(), cowboy_req:req()} |
	halt_return().
decode_base64(Context, CT, Req0) ->
	decode_base64(Context, CT, Req0, []).
decode_base64(Context, CT, Req0, Body) ->
	case cowboy_req:read_body(Req0) of
		{'error', 'badlength'} ->
			?MODULE:halt(Req0
									 ,cb_context:add_validation_error(<<"file">>, <<"maxLength">>
																										,<<"File was larger than allowed">>
																										,Context
																									 ));
		{'error', E} ->
			?MODULE:halt(Req0
									 ,cb_context:set_resp_status(
											cb_context:set_resp_data(Context, E)
											,'fatal'
										 ));
		{'more', _, Req1} ->
			handle_max_filesize_exceeded(Context, Req1);
		{'ok', Base64Data, Req1} ->
			Data = iolist_to_binary(lists:reverse([Base64Data | Body])),

			{EncodedType, FileContents} = kz_attachment:decode_base64(Data),
			ContentType = case EncodedType of
											'undefined' -> CT;
											<<"application/base64">> -> <<"application/octet-stream">>;
											Else -> Else
										end,
			Headers = wh_json:from_list([{<<"content_type">>, ContentType}
																	 ,{<<"content_length">>, byte_size(FileContents)}
																	]),
			FileJObj = wh_json:from_list([{<<"headers">>, Headers}
																		,{<<"contents">>, FileContents}
																	 ]),
			FileName = <<"uploaded_file_"
									 ,(wh_util:to_binary(wh_util:current_tstamp()))/binary
								 >>,
			{cb_context:set_req_files(Context, [{FileName, FileJObj}]), Req1}
	end.

-spec get_request_body(cowboy_req:req()) ->
	{binary(), cowboy_req:req()}.
-spec get_request_body(cowboy_req:req(), iolist()) ->
	{binary(), cowboy_req:req()}.
get_request_body(Req) ->
	get_request_body(Req, []).
get_request_body(Req0, Body) ->
	try get_request_body(Req0, Body, cowboy_req:read_part_body(Req0)) of
		Resp -> Resp
	catch
		'error':{'badmatch', _} ->
			get_request_body(Req0, Body, cowboy_req:read_body(Req0))
	end.

-type body_return() :: {'more', binary(), cowboy_req:req()} |
{'error', atom()} |
{'ok', binary(), cowboy_req:req()}.

-spec get_request_body(cowboy_req:req(), iolist(), body_return()) ->
	{binary(), cowboy_req:req()}.
get_request_body(Req0, _Body, {'error', _E}) ->
	{<<>>, Req0};
get_request_body(_Req0, _Body, {'more', _, Req1}) ->
	lager:error("file size exceeded, max is ~p", [?MAX_UPLOAD_SIZE]),
	{<<>>, Req1};
get_request_body(_Req0, Body, {'ok', Data, Req1}) ->
	{iolist_to_binary([Body, Data]), Req1}.

-type get_json_return() :: {wh_json:object(), cowboy_req:req()} |
{{'malformed', ne_binary()}, cowboy_req:req()}.
-spec get_json_body(cowboy_req:req()) -> get_json_return().
-spec decode_json_body(binary(), cowboy_req:req()) -> get_json_return().

get_json_body(Req0) ->
	{Body, Req1} = get_request_body(Req0),
	get_json_body(Body, Req1).

get_json_body(<<>>, Req) -> {wh_json:new(), Req};
get_json_body(ReqBody, Req) -> decode_json_body(ReqBody, Req).

decode_json_body(ReqBody, Req) ->
	try wh_json:decode(ReqBody) of
		JObj ->
			validate_decoded_json_body(normalize_envelope_keys(JObj), Req)
	catch
		'throw':{'invalid_json',{{'error',{ErrLine, ErrMsg}}, _JSON}} ->
			{{'malformed', <<(wh_util:to_binary(ErrMsg))/binary, " (around ", (wh_util:to_binary(ErrLine))/binary>>}, Req}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% validates decoded json body
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_decoded_json_body(wh_json:object(), cowboy_req:req()) -> get_json_return().
validate_decoded_json_body(JObj, Req) ->
	case is_valid_request_envelope(JObj) of
		'true' ->
			{JObj, Req};
		'false' ->
			{{'malformed', <<"Invalid JSON request envelope">>}, Req}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% normalizes envelope keys
%%   sets envelope keys to lowercase
%% @end
%%--------------------------------------------------------------------
-spec normalize_envelope_keys(wh_json:object()) -> wh_json:object().
normalize_envelope_keys(JObj) ->
	wh_json:foldl(fun normalize_envelope_keys_foldl/3, wh_json:new(), JObj).

-spec normalize_envelope_keys_foldl(wh_json:key(), wh_json:json_term(), wh_json:object()) -> wh_json:object().
normalize_envelope_keys_foldl(_K, 'undefined', JObj) -> JObj;
normalize_envelope_keys_foldl(K, V, JObj) -> wh_json:set_value(wh_json:normalize_key(K), V, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if the request envelope is valid
%% @end
%%--------------------------------------------------------------------
-spec is_valid_request_envelope(wh_json:object()) -> boolean().
is_valid_request_envelope(_JSON) ->
	%wh_json:get_value([<<"data">>], JSON, 'undefined') =/= 'undefined'. //huet
	true. 

-spec get_http_verb(http_method(), cb_context:context()) -> ne_binary().
get_http_verb(Method, Context) ->
	case cb_context:req_value(Context, <<"verb">>) of
		'undefined' -> Method;
		Verb ->
			wh_util:to_upper_binary(Verb)
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------

-type cb_mod_with_tokens() :: {ne_binary(), path_tokens()}.
-type cb_mods_with_tokens() :: [cb_mod_with_tokens(),...] | [].
-spec parse_path_tokens(cb_context:context(), path_tokens()) -> cb_mods_with_tokens().
parse_path_tokens(Context, Tokens) ->
	parse_path_tokens(Context, Tokens, []).

-spec parse_path_tokens(cb_context:context(), wh_json:keys(), cb_mods_with_tokens()) ->
	cb_mods_with_tokens().
parse_path_tokens(_, [], Events) -> Events;
parse_path_tokens(_, [<<>>], Events) -> Events;

parse_path_tokens(Context, [Mod|T], Events) ->
	case is_cb_module(Context, Mod) of
		'false' -> [];
		'true' ->
			{Params, List2} = lists:splitwith(fun(Elem) -> not is_atom(Context, Elem) end, T),
			parse_path_tokens(Context, List2, [{Mod, Params} | Events])
	end.

-spec is_cb_module(cb_context:context(), ne_binary()) -> boolean().
is_cb_module(_Context, _Elem) ->
	true.

-spec is_atom(cb:context(), ne_binary()) -> boolean().
is_atom(_Context, _Elem) ->
	false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, they can not add.
%%
%% If a client passes a ?verb=(PUT|DELETE) on a POST request, ReqVerb will
%% be ?HTTP_PUT or ?HTTP_DELETE, while HttpVerb is 'POST'. If the allowed
%% methods do not include 'POST', we need to add it if allowed methods include
%% the verb in ReqVerb.
%% So, POSTing a ?HTTP_PUT, and the allowed methods include 'PUT', insert POST
%% as well.
%% POSTing a ?HTTP_DELETE, and 'DELETE' is NOT in the allowed methods, remove
%% 'POST' from the allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allow_methods([http_methods(),...], ne_binary(), http_method()) -> http_methods().
allow_methods(Responses, _ReqVerb, _HttpVerb) ->
	case Responses of 
		[] -> [];
		Succeeded ->
			AllowedSet = lists:foldr(fun allow_methods_fold/2, sets:new(), Succeeded),
			sets:to_list(AllowedSet)
	end.

-spec allow_methods_fold(http_methods(), set:set()) -> set:set().
allow_methods_fold(Response, Acc) ->
	sets:union(Acc, sets:from_list(uppercase_all(Response))).

-spec uppercase_all(ne_binaries() | atoms()) -> ne_binaries().
uppercase_all(L) when is_list(L) ->
	[wh_util:to_upper_binary(wh_util:to_binary(I)) || I <- L].

%% insert 'POST' if Verb is in Allowed; otherwise remove 'POST'.
-spec maybe_add_post_method(ne_binary(), http_method(), http_methods()) -> http_methods().
maybe_add_post_method(?HTTP_POST, ?HTTP_POST, Allowed) ->
	Allowed;
maybe_add_post_method(Verb, ?HTTP_POST, Allowed) ->
	BigVerb = wh_util:to_upper_binary(Verb),
	case lists:member(BigVerb, Allowed) of
		'true' -> [?HTTP_POST | Allowed];
		'false' -> lists:delete(?HTTP_POST, Allowed)
	end;
maybe_add_post_method(_, _, Allowed) ->
	Allowed.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec is_authentic(cowboy_req:req(), cb_context:context()) ->
	{{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()} |
	halt_return().

is_authentic(Req, Context) ->
	is_authentic(Req, Context, cb_context:req_verb(Context)).

-spec is_authentic(cowboy_req:req(), cb_context:context(), http_method()) ->
	{{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()} |
	halt_return().
is_authentic(Req, Context, ?HTTP_OPTIONS) ->
	%% all OPTIONS, they are harmless (I hope) and required for CORS preflight
	{'true', Req, Context};
is_authentic(Req0, Context0, _ReqVerb) ->

	{Req1, Context1} = get_auth_token(Req0, Context0),
	is_authentic(Req1, Context1, _ReqVerb, cb_context:req_nouns(Context1)).


-spec is_authentic(cowboy_req:req(), cb_context:context(), http_method(), list()) ->
	{{'false', <<>>} | 'true', cowboy_req:req(), cb_context:context()} |
	halt_return().

is_authentic(Req, Context, _ReqVerb, []) ->
	% lager:info("authen: failed to authenticate"),
	?MODULE:halt(Req, cb_context:add_system_error('invalid_credentials', Context));

is_authentic(Req, Context, _ReqVerb, [{Mod, Params} | _ReqNouns]) ->
	Event = api_util:create_event_name(Context, <<"authenticate.", Mod/binary>>),
	Payload = [Context | Params],
	Responses = crossbar_bindings:map(Event, Payload),
	%lager:debug("emnvn is_authentic Event: ~p, Response: ~p~n",[Event, Responses]),
	case crossbar_bindings:succeeded(Responses) of
		[] ->
			%{'true', Req, Context}; %% HueT : TODO : process more
			?MODULE:halt(Req, cb_context:add_system_error('invalid_credentials', Context));
		['false'|_] -> 
			?MODULE:halt(Req, cb_context:add_system_error('invalid_credentials', Context));
		['true'|_] ->
			{true,  Req, Context};
		% 	prefer_new_context(T, Req, Context);
		[{'true', Context2}|_] ->
			{'true', Req, Context2};
		[{'halt', Context2}|_] ->
			?MODULE:halt(Req, Context2)
	end.


-spec get_auth_token(cowboy_req:req(), cb_context:context()) ->
	{cowboy_req:req(), cb_context:context()}.
get_auth_token(Req, Context) ->
	case cowboy_req:header(<<"authorization">>, Req) of
		'undefined'  ->
			case cb_context:req_value(Context, <<"auth_token">>) of
				'undefined' ->
					{Req, Context};
				Token ->
					{Req, cb_context:set_auth_token(Context, Token)}
			end;
		<<"Bearer ", Token/binary>> ->
			{Req, cb_context:set_auth_token(Context, Token)};
		<<"key=", KeyServer/binary>> ->
			%lager:info("Authorization KeyServer ~p ~n", [KeyServer]),
			{Req, cb_context:set_server_key(Context, KeyServer)};
		TokenHeader ->
			%lager:info("Authorization TokenHeader ~p ~n", [TokenHeader]),
			{Req, cb_context:set_req_headers(Context, [{<<"authorization">>, TokenHeader}])}
	end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec is_permitted(cowboy_req:req(), cb_context:context()) ->
	{'true', cowboy_req:req(), cb_context:context()} |
	halt_return().

is_permitted(Req, Context) ->
	is_permitted_verb(Req, Context, cb_context:req_verb(Context)).

-spec is_permitted_verb(cowboy_req:req(), cb_context:context(), http_method()) ->
	{'true', cowboy_req:req(), cb_context:context()} |
	halt_return().
is_permitted_verb(Req, Context, ?HTTP_OPTIONS) ->
	%% all all OPTIONS, they are harmless (I hope) and required for CORS preflight
	{'true', Req, Context};
is_permitted_verb(Req, Context0, _ReqVerb) ->
	Event = api_util:create_event_name(Context0, <<"authorize">>),
	Responses = crossbar_bindings:map(Event, Context0),
	lager:debug("emnvn is_permitted_verb Event: ~p, Response: ~p~n",[Event, Responses]),
	case crossbar_bindings:succeeded(Responses) of
		[] ->
			is_permitted_nouns(Req, Context0, _ReqVerb,cb_context:req_nouns(Context0));
		['true'|_] ->
			{'true', Req, Context0};
		[{'true', Context1}|_] ->
			{'true', Req, Context1};
		[{'halt', Context1}|_] ->
			?MODULE:halt(Req, Context1)
	end.

-spec is_permitted_nouns(cowboy_req:req(), cb_context:context(), http_method(), list()) ->
	{'true', cowboy_req:req(), cb_context:context()} |
	halt_return().
is_permitted_nouns(Req, Context, _ReqVerb, [{<<"404">>, []}]) ->
	?MODULE:halt(Req, cb_context:add_system_error('not_found', Context));
is_permitted_nouns(Req, Context, _ReqVerb, []) ->
	?MODULE:halt(Req, cb_context:add_system_error('forbidden', Context));
is_permitted_nouns(Req, Context0, _ReqVerb, [{Mod, Params} | _ReqNouns]) ->
	lager:debug("emnvn is_permitted_nouns 1 ~n",[]),
	Event = api_util:create_event_name(Context0, <<"authorize.", Mod/binary>>),
	Payload = [Context0 | Params],
	Result = crossbar_bindings:succeeded(crossbar_bindings:map(Event, Payload)),
	lager:debug("emnvn is_permitted_nouns 3: ~p ~n",[Result]),
	case Result of
		[] ->
			?MODULE:halt(Req, cb_context:add_system_error('forbidden', Context0));
		['true'|_] ->
			{'true', Req, Context0};
		[{'true', Context1}|_] ->
			{'true', Req, Context1};
		[{'halt', Context1}|_] ->
			?MODULE:halt(Req, Context1)
	end.

-spec is_known_content_type(cowboy_req:req(), cb_context:context()) ->
	{boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, Context) ->
	is_known_content_type(Req, Context, cb_context:req_verb(Context)).

is_known_content_type(Req, Context, ?HTTP_OPTIONS) ->
	{'true', Req, Context};
is_known_content_type(Req, Context, ?HTTP_GET) ->
	{'true', Req, Context};
is_known_content_type(Req, Context, ?HTTP_DELETE) ->
	{'true', Req, Context};
is_known_content_type(Req0, Context0, _ReqVerb) ->
	Context1 =
	lists:foldr(fun({Mod, Params}, ContextAcc) ->
									Event = api_util:create_event_name(Context0, <<"content_types_accepted.", Mod/binary>>),
									Payload = [ContextAcc | Params],
									crossbar_bindings:fold(Event, Payload)
							end, Context0, cb_context:req_nouns(Context0)),

	{CT, Req1} = get_content_type(Req0),

	is_known_content_type(Req1, Context1, ensure_content_type(CT), cb_context:content_types_accepted(Context1)).

-spec is_known_content_type(cowboy_req:req(), cb_context:context(), content_type(), list()) ->
	{boolean(), cowboy_req:req(), cb_context:context()}.
is_known_content_type(Req, Context, CT, []) ->
	is_known_content_type(Req, Context, CT, ?CONTENT_ACCEPTED);
is_known_content_type(Req, Context, CT, CTAs) ->
	CTA = lists:foldr(fun({_Fun, L}, Acc) ->
												lists:foldl(fun fold_in_content_type/2, Acc, L);
											 (L, Acc) ->
												lists:foldl(fun fold_in_content_type/2, Acc, L)
										end, [], CTAs),

	IsAcceptable = is_acceptable_content_type(CT, CTA),
	{IsAcceptable, Req, cb_context:set_content_types_accepted(Context, CTAs)}.

-spec fold_in_content_type({ne_binary(), ne_binary()}, list()) -> list().
fold_in_content_type({Type, Sub}, Acc) ->
	[{Type, Sub, []} | Acc].

-spec is_acceptable_content_type(content_type(), [content_type(),...] | []) -> boolean().
is_acceptable_content_type(CTA, CTAs) ->
	[ 'true' || ModCTA <- CTAs, content_type_matches(CTA, ModCTA)] =/= [].

%% (ReqContentType, ModuleContentType)
-spec content_type_matches(content_type(), content_type()) -> boolean().
content_type_matches({Type, _, _}, {Type, <<"*">>, []}) ->
	'true';
content_type_matches({Type, SubType, _}, {Type, SubType, []}) ->
	'true';
content_type_matches({Type, SubType, Opts}, {Type, SubType, ModOpts}) ->
	lists:all(fun({K, V}) ->
								props:get_value(K, Opts) =:= V
						end, ModOpts);
content_type_matches(CTA, {CT, SubCT, _}) when is_binary(CTA) ->
	CTA =:= <<CT/binary, "/", SubCT/binary>>;
content_type_matches(CTA, CT) when is_binary(CTA), is_binary(CT) ->
	CTA =:= CT;
content_type_matches(_CTA, _CTAs) ->
	'false'.

-spec ensure_content_type(content_type() | 'undefined') -> content_type().
ensure_content_type('undefined') -> ?CROSSBAR_DEFAULT_CONTENT_TYPE;
ensure_content_type(CT) -> CT.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------

-spec does_resource_exist(cb_context:context()) -> boolean().
-spec does_resource_exist(cb_context:context(), list()) -> boolean().
does_resource_exist(Context) ->
	does_resource_exist(Context, cb_context:req_nouns(Context)).

does_resource_exist(Context, [{Mod, Params}|_]) ->
	Event = api_util:create_event_name(Context, <<"resource_exists.", Mod/binary>>),
	Responses = crossbar_bindings:map(Event, Params),
	crossbar_bindings:any(Responses) and 'true';

does_resource_exist(_Context, _ReqNouns) ->
	'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function gives each {Mod, Params} pair a chance to determine if
%% it is valid and returns the status, and any errors
%%
%% validate_resource for each {Mod, Params} pair
%% validate for LAST {Mod, Params} pair
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), list()) -> cb_context:context().
validate(Context) ->
	validate(Context, cb_context:req_nouns(Context)).

validate(Context, ReqNouns) ->
	%% remove validate resource
	validate_data(Context, ReqNouns).


-spec validate_data(cb_context:context(), list()) -> cb_context:context().
validate_data(Context, [{Mod, Params}|_]) ->
	Event = api_util:create_event_name(Context, <<"validate.", Mod/binary>>),
	Payload = [cb_context:set_resp_status(Context, 'fatal') | Params],
	cb_context:import_errors(crossbar_bindings:fold(Event, Payload)).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec succeeded(cb_context:context()) -> boolean().
succeeded(Context) -> cb_context:resp_status(Context) =:='success'.

-spec execute_request(cowboy_req:req(), cb_context:context()) ->
	{boolean() | 'halt', cowboy_req:req(), cb_context:context()}.
-spec execute_request(cowboy_req:req(), cb_context:context(), ne_binary(), ne_binaries(), http_method()) ->
	{boolean() | 'halt', cowboy_req:req(), cb_context:context()}.
execute_request(Req, Context) ->
	case cb_context:req_nouns(Context) of
		[{Mod, Params}|_] ->
			execute_request(Req, Context, Mod, Params, cb_context:req_verb(Context));
		_ReqNouns ->
			{'false', Req, Context}
	end.

execute_request(Req, Context, Mod, Params, Verb) ->
	Event = create_event_name(Context, [<<"execute">>
																			,wh_util:to_lower_binary(Verb)
																			,Mod
																		 ]),

	Payload = [Context | Params],
	Context1 = crossbar_bindings:fold(Event, Payload),

	case cb_context:is_context(Context1) of
		'true' ->
			execute_request_results(Req, Context1, cb_context:resp_status(Context1));
		'false' ->
			execute_request_failure(Req, Context, Context1)
	end.

-spec execute_request_failure(cowboy_req:req(), cb_context:context(), any()) ->
	{'false', cowboy_req:req(), cb_context:context()}.
execute_request_failure(Req, Context, {'error', _E}) ->
	{'false', Req, Context};
execute_request_failure(Req, Context, _E) ->
	{'false', Req, Context}.

-spec execute_request_results(cowboy_req:req(), cb_context:context()) ->
	{'true' | 'halt', cowboy_req:req(), cb_context:context()}.
-spec execute_request_results(cowboy_req:req(), cb_context:context(), crossbar_status()) ->
	{'true' | 'halt', cowboy_req:req(), cb_context:context()}.
execute_request_results(Req, Context) ->
	case succeeded(Context) of
		'false' -> ?MODULE:halt(Req, Context);
		'true' -> {'true', Req, Context}
	end.

execute_request_results(Req, Context, 'success') ->
	execute_request_results(Req, Context);
execute_request_results(Req, Context, _RespStatus) ->
	?MODULE:halt(Req, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function runs the request terminated bindings at the conclusion
%% of all requests
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cowboy_req:req(), cb_context:context()) -> 'ok'.
finish_request(_Req, Context) ->
	[{Mod, _}|_] = cb_context:req_nouns(Context),
	Verb = cb_context:req_verb(Context),
	Event = create_event_name(Context, [<<"finish_request">>, Verb, Mod]),
	_ = spawn('crossbar_bindings', 'map', [Event, Context]),
	'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec create_resp_content(cowboy_req:req(), cb_context:context()) -> term().
	%{ne_binary() | iolist(), cowboy_req:req()}.
create_resp_content(Req0, Context) ->
	case is_raw_req(Context) of  %% HueT 
		false ->
								try wh_json:encode(create_resp_envelope(Context)) of
									JSON ->
										case cb_context:req_value(Context, <<"jsonp">>) of
											'undefined' -> {JSON, Req0};
											JsonFun when is_binary(JsonFun) ->
												{[JsonFun, <<"(">>, JSON, <<");">>]
												 ,cowboy_req:set_resp_header(<<"content-type">>, ?JSONP_CONTENT_TYPE, Req0)
												}
										end
								catch
									'throw':{'json_encode', {'bad_term', _Term}} ->
										{<<"encoding response failed: bad term">>, Req0};
									_E:_R ->
									ST = erlang:get_stacktrace(),
										lager:error("=======Error: Err1: ~p, Err2: ~p, ST: ~p~n",[_E,_R, ST]),
										{<<"failure in request, contact support 2">>, Req0}
								end;
		true -> 
			{create_resp_envelope(Context), Req0}

			
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
-spec create_push_response(cowboy_req:req(), cb_context:context()) ->
	{boolean(), cowboy_req:req(), cb_context:context()}.
create_push_response(Req0, Context) ->
	{Content, Req1} = create_resp_content(Req0, Context),
	Req2 = set_resp_headers(Req1, Context),
	{succeeded(Context), cowboy_req:set_resp_body(Content, Req2), Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-spec create_pull_response(cowboy_req:req(), cb_context:context()) ->
	{text(), cowboy_req:req(), cb_context:context()} |
	halt_return().
create_pull_response(Req0, Context) ->
	{Content, Req1} = create_resp_content(Req0, Context),
	Req2 = set_resp_headers(Req1, Context),
	case succeeded(Context) of
		'false' -> ?MODULE:halt(Req2, Context);
		'true' -> {Content, Req2, Context}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse fields and puts them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec create_resp_envelope(cb_context:context()) -> wh_json:object().
-spec do_create_resp_envelope(any(), cb_context:context()) -> wh_json:object().
create_resp_envelope(Context) ->
	IsRawReq = is_raw_req(Context),
	do_create_resp_envelope(IsRawReq,cb_context:import_errors(Context)).

do_create_resp_envelope(false, Context) ->

	Resp = case cb_context:response(Context) of
		{'ok', RespData} ->
						 case is_pivot_req(Context) of  %% HueT 
							 true ->
								 %% TODO :  response  with pivot commands and collected request
								 RespData;
							 _ ->
								 %% TODO :  response with normal request
								 ExtraData = cb_context:resp_extra(Context),
								 [{<<"auth_token">>, cb_context:auth_token(Context)}
									%,{<<"total">>, cb_context:total(Context)}
									,{<<"status">>, <<"success">>}
									,{<<"request_id">>, cb_context:req_id(Context)}
									% ,{<<"revision">>, wh_util:to_binary(cb_context:resp_etag(Context))}
									,{<<"data">>, RespData}| ExtraData
								 ]
						 end;
		{'error', {ErrorCode, ErrorMsg, RespData}} ->
					lager:debug("emnvn Error  NewRespData: ~p~n",[RespData]),

						 [{<<"auth_token">>, wh_util:to_binary(cb_context:auth_token(Context))}
							,{<<"request_id">>, cb_context:req_id(Context)}
							,{<<"status">>, <<"error">>}
							,{<<"message">>, ErrorMsg}
							,{<<"error">>, wh_util:to_binary(ErrorCode)}
							,{<<"data">>, RespData}
							,{<<"errors_data">>, format_error_responses([],RespData)}
						 ]
	end,
	wh_json:set_values(
		props:filter_undefined(Resp)
		,cb_context:resp_envelope(Context)
	 );

do_create_resp_envelope(_, Context) ->
	{'ok', RespData}  = cb_context:response(Context),
	lager:debug("RespData: ~p~n",[RespData]),
	RespData.

format_error_responses(CurrentFormatedErrrors, []) -> CurrentFormatedErrrors;

format_error_responses(CurrentFormatedErrrors, [Error|OtherErrors]) -> 
{Key,
 [{Type,Msg}]} = Error,
FormatedErrror = #{
    key => Key,
    type => Type,
    message => Msg
},
format_error_responses([FormatedErrror|CurrentFormatedErrrors], OtherErrors);

format_error_responses(CurrentFormatedErrrors, _) -> CurrentFormatedErrrors.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate through #cb_context.resp_headers, setting the headers specified
%% @end
%%--------------------------------------------------------------------
-spec set_resp_headers(cowboy_req:req(), cb_context:context() | wh_proplist()) ->
	cowboy_req:req().
set_resp_headers(Req0, []) -> Req0;
set_resp_headers(Req0, [_|_]=Headers) ->
	lists:foldl(fun({Header, Value}, ReqAcc) ->
									{H, V} = fix_header(Header, Value, ReqAcc),
									cowboy_req:set_resp_header(H, V, ReqAcc)
							end, Req0, props:filter_empty(Headers));
set_resp_headers(Req0, Context) ->
	set_resp_headers(Req0, cb_context:resp_headers(Context)).

-spec fix_header(text(), text(), cowboy_req:req()) ->
	{binary(), binary()}.
fix_header(<<"Location">> = H, Path, Req) ->
	{H, crossbar_util:get_path(Req, Path)};
fix_header(H, V, _) ->
	{wh_util:to_binary(H), wh_util:to_binary(V)}.

-spec halt(cowboy_req:req(), cb_context:context()) ->
	halt_return().
halt(Req0, Context) ->
	
	StatusCode = cb_context:resp_error_code(Context),
	lager:debug("emnvn halt resp. StatusCode: ~p ~n",[StatusCode]),
	{Content, Req1} = create_resp_content(Req0, Context),
	Req2 = cowboy_req:set_resp_body(Content, Req1),
	Req3 = ?MODULE:add_cors_headers(Req2, Context),
	Req31 = maybe_set_json_content_type(Req3),
	Req4 = cowboy_req:reply(StatusCode, Req31),
	{'stop', Req4, Context}.

-spec maybe_set_json_content_type(cowboy_req:req()) -> cowboy_req:req().
maybe_set_json_content_type(Req) ->
	ContentType = <<"content-type">>,
	case cowboy_req:resp_header(ContentType, Req, <<>>) of
		<<>> -> 
			lager:debug("No ~p header. Set to application/json ~n",[ContentType]),
			cowboy_req:set_resp_header(ContentType,<<"application/json">>, Req);
		_ -> 
			Req
	end.

%% Used
-spec create_event_name(cb_context:context(), ne_binary() | ne_binaries()) -> ne_binary().
create_event_name(Context, Segments) when is_list(Segments) ->
	create_event_name(Context, wh_util:join_binary(Segments, <<".">>));
create_event_name(Context, Name) ->
	ApiVersion = cb_context:api_version(Context),
	<<ApiVersion/binary, "_resource.", Name/binary>>.

validate_error(Context, FName, Type, Msg) ->
	cb_context:add_validation_error(FName, Type, wh_json:from_list([{<<"message">>, Msg}]), Context).

validate_error(Context, FName, Code, Type, Msg) ->
	cb_context:add_validation_error([FName, Code], Type, wh_json:from_list([{<<"message">>, Msg}]), Context).

validate_error(ErrCode, Context, Key, OriginValue, Type, Msg) ->
	cb_context:add_validation_error([Key, OriginValue, ErrCode], Type, wh_json:from_list([{<<"message">>, Msg}]), Context).

check_val(Context, Key, Val) ->
	case Val of
			<<>> ->
					api_util:validate_error(Context, Key, <<"required">>, <<"Field '", Key/binary, "' is required">>);
			_ ->
					Context
	end.

check_number_val(Context, Key, Val) ->
			case is_number(Val) of
				true -> Context;
				_ ->
				validate_error(Context, Key, <<"invalid">>, <<"Invalid '", Key/binary, "' must be string integer">>)
			end.

check_vals(Context, Key, Val, DefinedValues) ->
	case lists:member(Val, DefinedValues) of
		true ->
			Context;
		_ ->
			validate_error(400, Context, Key, <<>>, <<"invalid_",Key/binary>>, <<"Invalid ",Key/binary>>)
	end.

check_vals(Context, Key, Val) ->
	case is_list(Val) andalso Val /= [] of
		true ->
			Context;
		_ ->
			validate_error(400, Context, Key, <<>>, <<"invalid_",Key/binary>>, <<"Invalid ",Key/binary>>)
	end.

check_val_one_of(Context, Key, OriginValue, OneOfValues) ->
	case lists:member(OriginValue, OneOfValues) of
		true ->
			Context
			;
		_ ->
			validate_error(400, Context, Key, OriginValue, <<"invalid_",Key/binary>>, <<"Invalid '",Key/binary, "' field">>)
	end.

check_arr_val(Context, Key, Vals) ->
	case Vals of
		[] ->
			api_util:validate_error(Context, Key, <<"required">>, <<"Field '", Key/binary, "' is required">>);
		ArrVals when is_list(ArrVals) ->
			Context ;
		_ ->
			api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid '", Key/binary, "' .'", Key/binary, "' must be list">>)
	end.  
  
trans_props(MapVal) when is_map(MapVal) ->
	maps:to_list(MapVal); %[]
trans_props(Val) when is_list(Val) ->
	Val.

-spec is_pivot_req(cb_context:context()) -> boolean().
is_pivot_req(Context) ->
    Path = cb_context:path_tokens(Context),
    PathSet = sets:from_list(Path),
    PivotSet = sets:from_list(?PIVOT_TYPE),
    Common = sets:to_list(sets:intersection(PathSet, PivotSet)),
    case Common of
        [] ->
            false;
        _ ->
            true
    end.


-spec is_raw_req(cb_context:context()) -> boolean().
is_raw_req(Context) ->
    Path = cb_context:path_tokens(Context),
    PathSet = sets:from_list(Path),
    PivotSet = sets:from_list(?RAW_TYPE),
    Common = sets:to_list(sets:intersection(PathSet, PivotSet)),
    case Common of
        [] ->
            false;
        _ ->
            true
    end.

add_error_response(Code, ErrType, ErrorMsg) when is_binary(ErrType) ->
 add_error_response(Code, [ErrType], ErrorMsg);

add_error_response(Code, ErrTypes, ErrorMsg) ->
	[#{<<"error_code">> => integer_to_binary(Code),
		 <<"error">> => ErrTypes,
		 <<"message">> => ErrorMsg}].

add_error_response(Key, OriginValue, Code, Type, ErrorMsg) ->
	[#{<<"field">> => Key,
		 <<"error_code">> => integer_to_binary(Code),
		 <<"original_value">> => OriginValue,
		 <<"type">> => Type,
		 <<"message">> => ErrorMsg}].

validate_status_app(Context, Key, Val) ->
  case lists:member(Val, [?ACTIVE, ?INACTIVE]) of
    true -> Context;
    _ ->
      api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid status. Status must be active/inactive">>)
  end.
