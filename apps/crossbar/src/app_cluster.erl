-module(app_cluster).

%% API
-export([get_nodes/0, call/4]).
-export([join/1, leave/1]).

-spec get_nodes() -> [node()].

get_nodes() ->
    mnesia:system_info(running_db_nodes).

-spec call(node(), module(), atom(), [any()]) -> any().

call(Node, Module, Function, Args) ->
    rpc:call(Node, Module, Function, Args, 5000).



-spec join(node()) -> ok | {error, any()}.

join(Node) ->
    case {node(), net_adm:ping(Node)} of
        {Node, _} ->
            {error, {not_master, Node}};
        {_, pong} ->
            application:stop(crossbar),
            application:stop(mnesia),
            mnesia:delete_schema([node()]),
            application:start(mnesia),
            mnesia:change_config(extra_db_nodes, [Node]),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            spawn(fun()  ->
                lists:foreach(fun(Table) ->
                            Type = call(Node, mnesia, table_info, [Table, storage_type]),
                            mnesia:add_table_copy(Table, node(), Type)
                    end, mnesia:system_info(tables)--[schema])
                end),
            application:start(crossbar);
        _ ->
            {error, {no_ping, Node}}
    end.

-spec leave(node()) -> ok | {error, any()}.

leave(Node) ->
    case {node(), net_adm:ping(Node)} of
        {Node, _} ->
            Cluster = get_nodes()--[Node],
            leave(Cluster, Node);
        {_, pong} ->
            rpc:call(Node, ?MODULE, leave, [Node], 10000);
        {_, pang} ->
            case mnesia:del_table_copy(schema, Node) of
                {atomic, ok} -> ok;
                {aborted, Reason} -> {error, Reason}
            end
    end.
leave([], Node) ->
    {error, {no_cluster, Node}};
leave([Master|_], Node) ->
    application:stop(crossbar),
    application:stop(mnesia),
    call(Master, mnesia, del_table_copy, [schema, Node]),
    spawn(fun() ->
                mnesia:delete_schema([node()]),
                erlang:halt(0)
        end),
    ok.

