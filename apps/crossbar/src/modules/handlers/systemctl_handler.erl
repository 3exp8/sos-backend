-module(systemctl_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    handle_cluster/2,
    handle_init_data/2,
    list_nodes/0,
    validate_cluster_action/2,
    validate_cluster_node/2,
    validate_initdata_data/2,
    validate_initdata_data_type/2
]).

handle_cluster(?CLUSTER_ACTION_JOIN,ReqJson) -> 
    Node = zt_util:to_atom(wh_json:get_value(<<"node">>,ReqJson)),
    MyNode = erlang:node(),
    case Node of 
        MyNode -> {error, same_node};
    _ ->  app_cluster:join(Node)
    end;

handle_cluster(?CLUSTER_ACTION_LEAVE,ReqJson) -> 
    Node = zt_util:to_atom(wh_json:get_value(<<"node">>,ReqJson)),
    MyNode = erlang:node(),
    case Node of 
        MyNode -> {error, same_node};
    _ ->  app_cluster:leave(Node)
    end.

handle_init_data(<<"user_admin">>,UserData) ->
    case user_db:find_by_conditions([{role_id,?USER_ROLE_ADMIN}],[],1,0) of 
        [] ->
            hanldle_init_user(?USER_ROLE_ADMIN, UserData);
        _ -> {error, existed}
    end;

handle_init_data(<<"user_system">>,UserData) -> 
    case user_db:find_by_conditions([{role_id,?USER_ROLE_SYSTEM}],[],1,0) of 
        [] ->
            hanldle_init_user(?USER_ROLE_SYSTEM, UserData);
        _ -> {error, existed}
    end.

hanldle_init_user(Role, ReqJson) -> 
    BaseUserInfo = cb_user:get_user_info(ReqJson),
    Uuid = zt_util:get_uuid(),
    UserInfo = 
        maps:merge(BaseUserInfo, #{
            id => <<"user", Uuid/binary>>,
            status => ?USER_STATUS_ACTIVE,
            role => Role
        }),
    user_db:save(UserInfo).

list_nodes() -> 
    app_cluster:get_nodes().

-spec validate_cluster_action(api_binary(),  cb_context:context()) -> cb_context:context().
validate_cluster_action(ReqJson, Context) ->
    Key = <<"action">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    case lists:member(Val, ?CLUSTER_ACTION_TYPES) of
        true -> Context;
        _ ->
            Vals = zt_util:arr_to_str(?CLUSTER_ACTION_TYPES),
            api_util:validate_error(Context, Key, <<"invalid">>, <<"Invalid ",Key/binary,". Value must be ",Vals/binary>>)
    end.

-spec validate_cluster_node(api_binary(),  cb_context:context()) -> cb_context:context().
validate_cluster_node(ReqJson, Context) ->
    Key = <<"node">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    api_util:check_val(Context, Key, Val).

-spec validate_initdata_data(api_binary(),  cb_context:context()) -> cb_context:context().
validate_initdata_data(ReqJson, Context) ->
    Key = <<"data">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    api_util:check_val(Context, Key, Val).

-spec validate_initdata_data_type(api_binary(),  cb_context:context()) -> cb_context:context().
validate_initdata_data_type(ReqJson, Context) ->
    Key = <<"data_type">>,
    Val = wh_json:get_value(Key, ReqJson, <<>>),
    api_util:check_val(Context, Key, Val).


