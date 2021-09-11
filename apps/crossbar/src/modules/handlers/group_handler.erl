-module(group_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    find_groups_by_user/1,
    validate_type/2,
    validate_add_members/2,
    validate_remove_members/2,
    validate_name/2,
    validate_contact_info/2,
    validate_detail_info/2,
    validate_verify_status/2
]).

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
                    case {
                        proplists:get_value(<<"id">>,Val,<<>>),
                        proplists:get_value(<<"role">>,Val,<<>>)
                    }
                        of 
                            {<<>>, _} -> false;
                            {_, <<>>} -> false;
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