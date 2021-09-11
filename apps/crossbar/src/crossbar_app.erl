%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(crossbar_app).

-behaviour(application).

%% hue: -include_lib("whistle/include/wh_types.hrl").

%% Application callbacks
-export([start/2, stop/1]).

-include("crossbar.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
%%-spec start(term(), term()) ->
%%                   {'ok', pid()} |
%%                   {'ok', pid(), term()} |
%%                   {'error', startlink_err()}.
start(_StartType, _StartArgs) ->
	db_init(),
	 _ = start_deps(),
	 init_mnesia_tables(),
	app_handle_commands:init(),
	crossbar_sup:start_link().
	% crossbar:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) -> crossbar:stop().

db_init() ->
	MyNode = node(),
	DbNodes = mnesia:system_info(db_nodes),
	case lists:member(MyNode, DbNodes) of
	true -> ok;
	false ->
		lager:critical("Node name mismatch: I'm [~s], "
				"the database is owned by ~p", [MyNode, DbNodes]),
		lager:critical("Either set ERLANG_NODE in ejabberdctl.cfg "
				"or change node name in Mnesia", []),
		erlang:error(node_name_mismatch)
	end,
	case mnesia:system_info(extra_db_nodes) of
	[] ->
		mnesia:create_schema([node()]);
	_ ->
		ok
	end,
	start_app(mnesia, permanent),
	mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).


start_app(App, Type) ->
	StartFlag = not is_loaded(),
	start_app([App], Type, StartFlag).

start_app([App|Apps], Type, StartFlag) ->
	case application:start(App,Type) of
	ok ->
		spawn(fun() -> check_app_modules(App, StartFlag) end);
	{error, {already_started, _}} ->
		ok;
	{error, {not_started, DepApp}} ->
		case lists:member(DepApp, [App|Apps]) of
		true ->
			Reason = io_lib:format(
						 "failed to start application '~p': "
						 "circular dependency on '~p' detected",
						 [App, DepApp]),
			exit_or_halt(Reason, StartFlag);
		false ->
			start_app([DepApp, App|Apps], Type, StartFlag)
		end;
	Err ->
		Reason = io_lib:format("failed to start application '~p': ~p",
								 [App, Err]),
		exit_or_halt(Reason, StartFlag)
	end;

start_app([], _Type, _StartFlag) ->
	ok.


is_loaded() ->
	Apps = application:which_applications(),
	lists:keymember(crossbar, 1, Apps).

check_app_modules(App, StartFlag) ->
	{A, B, C} = os:timestamp(),
	random:seed(A, B, C),
	sleep(5000),
	case application:get_key(App, modules) of
	{ok, Mods} ->
		lists:foreach(fun(Mod) ->
			case code:which(Mod) of
			non_existing ->
				File = get_module_file(App, Mod),
				Reason = io_lib:format(
						 "couldn't find module ~s "
						 "needed for application '~p'",
						 [File, App]),
				exit_or_halt(Reason, StartFlag);
			_ ->
				sleep(10)
			end
		end, Mods);
	_ ->
		%% No modules? This is strange
		ok
	end.

exit_or_halt(Reason, StartFlag) ->
	lager:critical(Reason, []),
	if StartFlag ->
					%% Wait for the critical message is written in the console/log
					timer:sleep(1000),
					halt(string:substr(lists:flatten(Reason), 1, 199));
		 true ->
					erlang:error(application_start_failed)
	end.

get_module_file(App, Mod) ->
	BaseName = atom_to_list(Mod),
	case code:lib_dir(App, ebin) of
	{error, _} ->
		BaseName;
	Dir ->
		filename:join([Dir, BaseName ++ ".beam"])
	end.

sleep(N) ->
	timer:sleep(random:uniform(N)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(crossbar), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                                ,'public_key'
                                                ,'ssl'
                                                ,'inets'
                                                ,'lager'
                                                ,'ranch'
                                                ,'cowlib'
                                                ,'cowboy'
                                                ,'worker_pool'
                                                ,'ibrowse'
												,'erlcloud'
                                               ]],
    'ok'.

init_mnesia_tables() ->
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  access_token_mnesia_db:create_table(),
  sos_cache:create_table().
