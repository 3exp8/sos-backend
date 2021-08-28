-module(app_admin).

-include("app_commands.hrl").

-export([start_link/0]).

-export([init/1
		, handle_call/3
		, handle_cast/2
		, handle_info/2
		, terminate/2
		, code_change/3]).

-export([join_cluster/1
		,leave_cluster/1
		,list_cluster/0
		,dump_mnesia/1
		,dump_table/2
		,load_mnesia/1]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    app_handle_commands:register_commands(get_commands_spec()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    app_handle_commands:unregister_commands(get_commands_spec()).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_commands_spec() ->
	[#app_commands{name = join_cluster, tags = [cluster],
			desc = "Join this node into the cluster handled by Node",
			module = ?MODULE, function = join_cluster,
			args_desc = ["Nodename of the node to join"],
			args_example = [<<"node1@machine7">>],
			args = [{node, binary}],
			result = {res, rescode}},
    #app_commands{name = leave_cluster, tags = [cluster],
			desc = "Remove and shutdown Node from the running cluster",
			longdesc = "This command can be run from any running node of the cluster, "
			    "even the node to be removed.",
			module = ?MODULE, function = leave_cluster,
			args_desc = ["Nodename of the node to kick from the cluster"],
			args_example = [<<"node1@machine8">>],
			args = [{node, binary}],
			result = {res, rescode}},

    #app_commands{name = list_cluster, tags = [cluster],
			desc = "List nodes that are part of the cluster handled by Node",
			module = ?MODULE, function = list_cluster,
			result_example = [node1@machine7, node2@machine8],
			args = [],
			result = {nodes, {list, {node, atom}}}},

	#app_commands{name = dump, tags = [mnesia],
			desc = "Dump the database to a text file",
			module = ?MODULE, function = dump_mnesia,
			args_desc = ["Full path for the text file"],
			args_example = ["rel/app/database.txt"],
			args = [{file, string}], result = {res, restuple}},

    #app_commands{name = dump_table, tags = [mnesia],
			desc = "Dump a table to a text file",
			module = ?MODULE, function = dump_table,
			args_desc = ["Full path for the text file", "Table name"],
			args_example = ["rel/app/table-transfer-doc.txt", "transfer_doc"],
			args = [{file, string}, {table, string}], result = {res, restuple}},

	#app_commands{name = load, tags = [mnesia],
			desc = "Restore the database from a text file",
			module = ?MODULE, function = load_mnesia,
			args_desc = ["Full path to the text file"],
			args_example = ["rel/app/database.txt"],
			args = [{file, string}], result = {res, restuple}}
		].

join_cluster(NodeBin) ->
    app_cluster:join(list_to_atom(binary_to_list(NodeBin))).


leave_cluster(NodeBin) ->
    app_cluster:leave(list_to_atom(binary_to_list(NodeBin))).


list_cluster() ->
    app_cluster:get_nodes().


dump_mnesia(Path) ->
    Tabs = get_local_tables(),
    dump_tables(Path, Tabs).

dump_table(Path, STable) ->
    Table = list_to_atom(STable),
    dump_tables(Path, [Table]).

load_mnesia(Path) ->
    case mnesia:load_textfile(Path) of
        {atomic, ok} ->
            {ok, ""};
        {error, Reason} ->
            String = io_lib:format("Can't load dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_load, String}
    end.

get_local_tables() ->
    Tabs1 = lists:delete(schema, mnesia:system_info(local_tables)),
    Tabs = lists:filter(
	     fun(T) ->
		     case mnesia:table_info(T, storage_type) of
			 disc_copies -> true;
			 disc_only_copies -> true;
			 _ -> false
		     end
	     end, Tabs1),
    Tabs.

dump_tables(Path, Tables) ->
    case dump_to_textfile(Path, Tables) of
	ok ->
	    {ok, ""};
	{error, Reason} ->
            String = io_lib:format("Can't store dump in ~p at node ~p: ~p",
				   [filename:absname(Path), node(), Reason]),
	    {cannot_dump, String}
    end.

dump_to_textfile(File, Tabs) ->
    dump_to_textfile(mnesia:system_info(is_running), Tabs, file:open(File, [write])).

dump_to_textfile(yes, Tabs, {ok, F}) ->
    Defs = lists:map(
	     fun(T) -> {T, [{record_name, mnesia:table_info(T, record_name)},
			    {attributes, mnesia:table_info(T, attributes)}]}
	     end,
	     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);

dump_to_textfile(_, _, {ok, F}) ->
    file:close(F),
    {error, mnesia_not_running};
dump_to_textfile(_, _, {error, Reason}) ->
    {error, Reason}.

dump_tab(F, T) ->
    W = mnesia:table_info(T, wild_pattern),
    {atomic,All} = mnesia:transaction(
		     fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(
      fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).