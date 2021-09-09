-module(group_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    find_groups_by_user/1
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