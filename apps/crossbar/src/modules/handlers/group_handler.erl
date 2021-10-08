-module(group_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    build_search_conditions/2,
    deformat_sorts/3,
    find_groups_by_user/1,
    is_group_member/2,
    is_group_admin/2,
    validate_type/2,
    validate_add_members/2,
    validate_remove_members/2,
    validate_name/2,
    validate_contact_info/2,
    validate_detail_info/2,
    validate_verify_status/2
]).

build_search_conditions(CurLocation, ReqJson) -> 
    RequetTypes = wh_json:get_value(<<"types">>, ReqJson, <<>>),
    VerifyStatus = wh_json:get_value(<<"verify_status">>, ReqJson, <<>>),
    Keyword = wh_json:get_value(<<"keyword">>, ReqJson, <<>>),
    Distance = zt_util:to_integer(wh_json:get_value(<<"distance">>, ReqJson, 10)),
    SearchRequests = [
        {types,RequetTypes},
        {verify_status,VerifyStatus},
        {keyword,Keyword}
    ],
    DistanceCond = {location,'distance',{CurLocation, Distance, <<"km">>}},
    lists:foldl(fun({Type,Cond},Acc) -> 
        case build_condition(Type, Cond) of 
            ignore -> Acc;
            NewCondtion -> [NewCondtion|Acc]
        end
    end,[DistanceCond],SearchRequests).

build_condition(_,<<>>) -> ignore;

build_condition(types, Val) -> 
    Vals = zt_util:split_string(Val),
    {type,'in',Vals};

build_condition(verify_status, Val) -> 
    {<<"verify_status">>,Val};

build_condition(keyword, Val) -> 
    {'or',[{<<"name">>,Val},{<<"description">>,Val}]};

build_condition(_,_) -> ignore.

deformat_sorts(Sorts, CurLocation, Unit) ->
    lists:map(fun (X) ->
        [Y] = X,
        {K, V} = Y,
        case K of
            <<"sort_distance">> ->
                {K, {CurLocation, V, Unit}};
            _ ->
                {K, V}
        end
    end, Sorts).

find_groups_by_user(<<>>) -> [];
find_groups_by_user(undefined) -> [];
find_groups_by_user(UserId) ->
    Groups = group_db:find_by_conditions(
        [
          {'or',[
                {<<"members.id">>,UserId},
                {<<"members#id">>,UserId}
              ]}],[],10,0),
    lists:map(fun(#{members := Members} = GroupInfo) -> 
                MemberInfo = maps:with([id, type, name], GroupInfo),
                maps:merge(MemberInfo, #{
                  role => user_handler:find_role(Members, UserId)
                })
    end,Groups).

is_group_member(GroupId, UserId) ->
    Conds = [
        {id, GroupId},
        {'or',[
            {<<"members.id">>,UserId},
            {<<"members#id">>,UserId}
        ]}
    ],
    case group_db:find_by_conditions(Conds,[],1,0) of
        [] -> false;
        _ -> true 
    end.

is_group_admin(GroupId, UserId) ->
    Conds = [
        {id, GroupId},
        {<<"members#role">>,?GROUP_USER_ROLE_ADMIN},
        {'or',[
            {<<"members.id">>,UserId},
            {<<"members#id">>,UserId}
        ]}
    ],
    case group_db:find_by_conditions(Conds,[],1,0) of
        [] -> false;
        _ -> true 
    end.
 
validate_type(ReqJson, Context) ->
    Type = wh_json:get_value(<<"type">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"type">>, Type).

validate_add_members(ReqJson, Context) ->
    Key = <<"members">>,
    case wh_json:get_value(Key, ReqJson, []) of
        [] -> 
            api_util:validate_error(Context, Key, <<"required">>, <<Key/binary," is required">>);
        Members when is_list(Members) ->
            AllMemberOk = 
                lists:all(fun(Val) ->
                    Role = proplists:get_value(<<"role">>,Val,<<>>),
                    Id = proplists:get_value(<<"id">>,Val,<<>>),
                    case lists:member(Role, ?GROUP_USER_ROLES) of
                        true -> 
                                case { Id, Role}  of 
                                    {<<>>, _} -> false;
                                    {_, <<>>} -> false;
                                    _ -> true 
                                end;
                        _ ->
                            false
                    end
                end,Members),
            case AllMemberOk of 
                true -> Context;
                false -> 
                    api_util:validate_error(Context, Key, <<"invalid">>, <<"One or more ",Key/binary," is not valid">>)
            end;
        _ -> 
            api_util:validate_error(Context, Key, <<"invalid">>, <<Key/binary," must be list">>)
    end.

validate_remove_members(ReqJson, Context) ->
    Key = <<"members">>,
    case wh_json:get_value(Key, ReqJson, []) of
        [] -> 
            api_util:validate_error(Context, Key, <<"required">>, <<Key/binary," is required">>);
        Members when is_list(Members) ->
            AllMemberOk = 
                lists:all(fun(Val) ->
                    case proplists:get_value(<<"id">>,Val,<<>>) of 
                        <<>> -> false;
                        _ -> true 
                    end
                end,Members),
            case AllMemberOk of 
                true -> Context;
                false -> 
                    api_util:validate_error(Context, Key, <<"invalid">>, <<"One or more ",Key/binary," is not valid">>)
            end;
        _ -> 
            api_util:validate_error(Context, Key, <<"invalid">>, <<Key/binary," must be list">>)
    end.
validate_name(ReqJson, Context) ->
    Subject = wh_json:get_value(<<"name">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"name">>, Subject).

validate_contact_info(ReqJson, Context) ->
    Content = wh_json:get_value(<<"contact_info">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"contact_info">>, Content).

validate_detail_info(ReqJson, Context) ->
    Content = wh_json:get_value(<<"detail_info">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"detail_info">>, Content).
validate_verify_status(ReqJson, Context) ->
    Type = wh_json:get_value(<<"verify_status">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"verify_status">>, Type).