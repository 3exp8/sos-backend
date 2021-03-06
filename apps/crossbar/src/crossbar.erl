%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(crossbar).

-export([start_link/0, start/0, stop/0
                 ,start_mod/1, stop_mod/1
                 ,get_server_key/1
                ]).

-include("crossbar.hrl").

-define(USE_COMPRESSION, kapps_config:get_is_true(?CONFIG_CAT, <<"compress_response_body">>, 'true')).

-spec crossbar_routes() -> cowboy_router:routes().
crossbar_routes() -> [{'_', paths_list()}].

paths_list() ->
    %[api_path(), default_path()].
    [api_path()].

%default_path() ->
%    {'_', 'crossbar_default_handler', []}.

api_path() ->
    {<<"/api/:version/[...]">>, [api_version_constraint()], 'api_resource', []}.
%{<<"/api/:version/[...]">>, 'api_resource', []}.

-spec api_version_constraint() -> cowboy_router:constraint().
api_version_constraint() ->
    {'version', fun api_version_constraint/2}.

%-spec api_version_constraint(ne_binary()) -> boolean().
api_version_constraint(forward, <<"v", ApiVersion/binary>>=Vsn) ->
    try wh_util:to_integer(ApiVersion) of
        Int ->
            lager:debug("routing to version ~b", [Int]),
            {'ok', Vsn}
            % 'true'
    catch
        _:_ ->
            lager:debug("not routing to version ~s", [ApiVersion]),
            {'error', 'not_a_version'}
    end;

api_version_constraint(_Type, Vsn) ->
    lager:debug("not routing to version ~s", [Vsn]),
    {'error', 'not_a_version'}.




%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------

-spec start_link() -> startlink_ret().
start_link() ->
    % put('callid', ?LOG_SYSTEM_ID),
    %%_ = declare_exchanges(),

    Dispatch = cowboy_router:compile(crossbar_routes()),

    maybe_start_plaintext(Dispatch),
    maybe_start_ssl(Dispatch),
    % OK = crossbar_sup:start_link(),
    % _ = crossbar_bindings:init(),
    % crossbar_handler_pool:init(),
    'ignore'.

start() ->
    application:start('crossbar').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    cowboy:stop_listener('api_resource'),
    cowboy:stop_listener('api_resource_ssl'),
    crossbar_bindings:flush(),
    % exit(whereis('crossbar_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec start_mod(atom() | string() | binary()) -> any().
start_mod(CBMod) when not is_atom(CBMod) -> start_mod(wh_util:to_atom(CBMod, 'true'));
start_mod(CBMod) -> CBMod:init().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Load a crossbar module's bindings into the bindings server
%% @end
%%--------------------------------------------------------------------
-spec stop_mod(atom() | string() | binary()) -> any().
stop_mod(CBMod) when not is_atom(CBMod) -> stop_mod(wh_util:to_atom(CBMod, 'true'));
stop_mod(CBMod) ->
    crossbar_bindings:flush_mod(CBMod),
    case erlang:function_exported(CBMod, 'stop', 0) of
        'true' -> CBMod:stop();
        'false' -> 'ok'
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
%-spec declare_exchanges() -> 'ok'.
%declare_exchanges() ->
%    _ = wapi_acdc_agent:declare_exchanges(),
%    _ = wapi_acdc_stats:declare_exchanges(),
%    _ = wapi_money:declare_exchanges(),
%    _ = wapi_notifications:declare_exchanges(),
%    _ = wapi_presence:declare_exchanges(),
%    _ = wapi_registration:declare_exchanges(),
%    _ = wapi_resource:declare_exchanges(),
%    _ = wapi_switch:declare_exchanges(),
%    _ = wapi_sysconf:declare_exchanges(),
%    _ = wapi_call:declare_exchanges(),
%    _ = wapi_dialplan:declare_exchanges(),
%    wapi_self:declare_exchanges().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Functions for onrequest and onresponse callbacks
%% @end
%%--------------------------------------------------------------------

-spec maybe_start_plaintext(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_plaintext(Dispatch) ->
    lager:info("start_app: Dispatch ~p ~n", [Dispatch]),
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_plaintext">>, 'true') of
        'false' -> lager:info("plaintext api support not enabled");
        'true' ->
            %Port = whapps_config:get_integer(?CONFIG_CAT, <<"port">>, 8080),
            Port = application:get_env(crossbar, port, 8080),
            ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10000),
            IdleTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"idle_timeout_ms">>, 120000),
            Workers = whapps_config:get_integer(?CONFIG_CAT, <<"workers">>, 100),

            %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
            % try cowboy:start_http('api_resource', Workers
            %                       ,[{'port', Port}]
            %                       ,[{'env', [{'dispatch', Dispatch}
            %                                  ,{'timeout', ReqTimeout}
            %                                 ]}
            %                         ,{'onrequest', fun on_request/1}
            %                         ,{'onresponse', fun on_response/4}
            %                        ]
            %                      ) of

            % transport_opts(SocketOpts) :: #{
            %     connection_type => worker | supervisor,
            %     handshake_timeout => timeout(),
            %     max_connections => max_conns(),
            %     logger => module(),
            %     num_acceptors => pos_integer(),
            %     num_conns_sups => pos_integer(),
            %     num_listen_sockets => pos_integer(),
            %     shutdown => timeout() | brutal_kill,
            %     socket_opts => SocketOpts
            % }.
            try
                lager:info("trying to bind to port ~b", [Port]),
                cowboy:start_clear('api_resource',
                                                     #{socket_opts => [{'port', Port}],
                                                         num_acceptors => Workers},
                                                     #{'env' => #{'dispatch' => Dispatch}
                                                         % ,'stream_handlers' => maybe_add_compression_handler()
                                                         ,'shutdown_timeout' => ReqTimeout
                                                         ,'idle_timeout' => IdleTimeout
                                                        })
            of
                {'ok', _} ->
                    lager:info("started plaintext API server");
                {'error', {'already_started', _P}} ->
                    lager:info("already started plaintext API server at ~p", [_P])
            catch
                _E:_R ->
                    lager:warning("crashed starting API server: ~s: ~p", [_E, _R])
            end
    end.

-spec maybe_start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
maybe_start_ssl(Dispatch) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_ssl">>, 'false') of
        'false' -> lager:info("ssl api support not enabled");
        'true' -> start_ssl(Dispatch)
    end.

-spec start_ssl(cowboy_router:dispatch_rules()) -> 'ok'.
start_ssl(Dispatch) ->
    try ssl_opts(code:lib_dir('crossbar')) of
        SSLOpts ->
            lager:info("trying to start SSL API server"),
            _SslStarted = ssl:start(),
            lager:info("starting SSL : ~p", [_SslStarted]),
            ReqTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"request_timeout_ms">>, 10000),
            Workers = whapps_config:get_integer(?CONFIG_CAT, <<"ssl_workers">>, 100),
            IdleTimeout = whapps_config:get_integer(?CONFIG_CAT, <<"idle_timeout_ms">>, 120000),
            % try cowboy:start_https('api_resource_ssl', Workers
            %                        ,SSLOpts
            %                        ,[{'env', [{'dispatch', Dispatch}
            %                                   ,{'timeout', ReqTimeout}
            %                                  ]}
            %                          ,{'onrequest', fun on_request/1}
            %                          ,{'onresponse', fun on_response/4}
            %                         ]
            %                       )
            % of
            try
                lager:info("trying to bind SSL API server  port ~b"
                                     ,[props:get_value('port', SSLOpts)]
                                    ),
                cowboy:start_tls('api_resource_ssl',
                                                 #{'socket_opts' => SSLOpts
                                                     ,'num_acceptors' => Workers
                                                    },
                                                 #{'env' => #{'dispatch' => Dispatch}
                                                     % ,'stream_handlers' => maybe_add_compression_handler()
                                                     ,'request_timeout' => ReqTimeout
                                                     ,'idle_timeout' => IdleTimeout
                                                     ,'logger' => 'lager'
                                                    }
                                                )
            of
                {'ok', _} ->
                    lager:info("started SSL API server on port ~b", [props:get_value('port', SSLOpts)]);
                {'error', {'already_started', _P}} ->
                    lager:info("already started SSL API server on port ~b at ~p"
                                         ,[props:get_value('port', SSLOpts), _P]
                                        )
            catch
                'throw':{'invalid_file', _File} ->
                    lager:info("SSL disabled: failed to find ~s", [_File]);
                _E:_R ->
                    lager:warning("crashed starting SSL API server: ~s: ~p", [_E, _R])
            end
    catch
        'throw':_E ->
            lager:warning("failed to start SSL API server: ~p", [_E])
    end.

-spec ssl_opts(list()) -> wh_proplist().
ssl_opts(RootDir) ->
    BaseOpts = base_ssl_opts(RootDir),
    case whapps_config:get_string(?CONFIG_CAT, <<"ssl_ca_cert">>) of
        'undefined' -> BaseOpts;
        SSLCACert -> [{'cacertfile', SSLCACert} | BaseOpts]
    end.

-spec base_ssl_opts(list()) -> wh_proplist().
base_ssl_opts(RootDir) ->
    [{'port', whapps_config:get_integer(?CONFIG_CAT, <<"ssl_port">>, 8443)}
     ,{'certfile', find_file(whapps_config:get_string(?CONFIG_CAT
                                                                                                        ,<<"ssl_cert">>
                                                                                                        ,filename:join([RootDir, <<"priv/ssl/crossbar.crt">>])
                                                                                                     ), RootDir)}
     ,{'keyfile', find_file(whapps_config:get_string(?CONFIG_CAT
                                                                                                     ,<<"ssl_key">>
                                                                                                     ,filename:join([RootDir, <<"priv/ssl/crossbar.key">>])
                                                                                                    ), RootDir)}
     ,{'password', whapps_config:get_string(?CONFIG_CAT, <<"ssl_password">>, <<>>)}
    ].

-spec find_file(list(), list()) -> list().
find_file(File, Root) ->
    case filelib:is_file(File) of
        'true' -> File;
        'false' ->
            FromRoot = filename:join([Root, File]),
            lager:info("failed to find file at ~s, trying ~s", [File, FromRoot]),
            case filelib:is_file(FromRoot) of
                'true' -> FromRoot;
                'false' ->
                    lager:info("failed to find file at ~s", [FromRoot]),
                    throw({'invalid_file', File})
            end
    end.

get_server_key(KN) ->
 ServerKeys = application:get_env(crossbar, server_key, <<>>),
 proplists:get_value(KN, ServerKeys, <<>>).

-spec maybe_add_compression_handler() -> [atom()].
maybe_add_compression_handler() ->
    case ?USE_COMPRESSION of
        'true' -> ['cowboy_compress_h', 'cowboy_stream_h'];
        'false' -> ['cowboy_stream_h']
    end.


