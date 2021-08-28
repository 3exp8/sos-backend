-module(db).
-export([start_link/0]).

%%% Exports for gen_server
-export([ init/1
					, handle_call/3
					, handle_cast/2
					, handle_info/2
					, terminate/2
					, code_change/3
					, reindex/0
					, reinit_data/0
				]).

-record(state, {}).

-type state() :: #state{}.

%%  Create index search and tie to bucket
start_link() ->
	lager:info("emnvn enter start_link function ",[]),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:info("emnvn===================== init enter init function ",[]),

	%init_sample_data(),
	timer:send_after(5000, self(), init_data),
	{ok, noreply}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(init_data, State) ->
	case user_db:find(<<"accountddfc2382b892fae1d4857b8cae670e73">>) of 
		notfound -> 
			lager:info("Data is nont initialized. Init first time!"),
			Backends = application:get_env(sumo_db, storage_backends, []),
			lager:info("Backends: ========~p",[Backends]),
			case Backends of
				[{_Name, sumo_backend_elasticsearch, _Options}|_] ->
					{ok, Docs} = application:get_env(sumo_db, docs),
					init_schema(Docs),
					lager:info("Initialized schema! Watiting for writing."),
					timer:sleep(10000),
					init_data(Docs),
					ok;
				true  -> ok
			end;
		_ -> lager:info("Data is initialized.")
	end,
	{noreply, State};


handle_info(_Msg, State) -> {noreply, State}.

init_schema(Docs) -> 
	lists:foreach(fun({DocName, _}) ->
		try
			case erlang:function_exported(DocName,create_schema,0) of
					true ->
						Res = DocName:create_schema(),
						lager:info("Created index: ~p for doc: ~p~n",[DocName, Res]);
					_ ->
						lager:warning("~p:create_schema() is not defined ~n",[DocName])
			end
		catch
			_E:_R ->
				lager:warning("Error when int data DocName: ~p~n",[DocName])
		end
	end, Docs).

init_data(Docs) -> 
	lists:foreach(fun({DocName, _}) ->
		try
			case erlang:function_exported(DocName,init_data,0) of
				true ->
					Res2 = DocName:init_data(),
					lager:info("Initalized data: ~p for doc: ~p~n",[DocName, Res2]);
				_ ->
					lager:warning("~p:Init_data() is not defined ~n",[DocName])
			end
		catch
			_E:_R ->
				lager:warning("Error when int data DocName: ~p~n",[DocName])
		end
	end, Docs).



-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



riak_opts(Options) ->
	User = proplists:get_value(username, Options),
	Pass = proplists:get_value(password, Options),
	Opts0 = case User /= undefined andalso Pass /= undefined of
						true -> [{credentials, User, Pass}];
						_    -> []
					end,
	Opts1 = case lists:keyfind(connect_timeout, 1, Options) of
						{_, V1} -> [{connect_timeout, V1}, {auto_reconnect, true}] ++ Opts0;
						_       -> [{auto_reconnect, true}] ++ Opts0
					end,
	Opts1.

% Db start doc:
% role_doc
% restaurant_doc
% user_doc

reindex() ->
	DocModels = get_doc_models(),
	DbModels = [notification_db, promotion_db, restaurant_db, role_db, transaction_fee_db, user_db, wallet_db],
	%% Delete data
	DelFun = fun (DbModel) ->  DbModel:del_all() end,
	SchemaFun = fun (DocModel) -> DocModel:delete_schema(),
																DocModel:create_schema()	end,
	lists:foreach(DelFun, DbModels),
	lists:foreach(SchemaFun, DocModels).

reinit_data() ->
	DocModels = get_doc_models(),
	SchemaFun = fun (DocModel) -> DocModel:init_data()	end,
	lists:foreach(SchemaFun, DocModels).

get_doc_models() ->
	[notification_doc, promotion_doc, restaurant_doc, role_doc, transaction_fee_doc, user_doc, wallet_doc].
