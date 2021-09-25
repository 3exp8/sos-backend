-module(cb_news).
-include("crossbar.hrl").

-export([init/0,
         allowed_methods/0,
         allowed_methods/1,
         allowed_methods/2,
         resource_exists/0,
         resource_exists/1,
         resource_exists/2,
         authenticate/1,
         authenticate/2,
         authenticate/3,
         authorize/1,
         authorize/2,
         authorize/3,
         validate/1,
         validate/2,
         validate/3
        ]).

-export([
    handle_get/1, 
    handle_get/2, 
    handle_put/1, 
    handle_post/2,
    handle_post/3, 
    handle_delete/2
]).

-export([
          permissions/0
  ]).

init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.news">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"*.resource_exists.news">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"*.authenticate.news">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"*.authorize.news">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"*.authorize_verb.news">>, ?MODULE, authorize_verb),
    _ = crossbar_bindings:bind(<<"*.validate.news">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"*.to_json.get.news">>, ?MODULE, handle_get),
    _ = crossbar_bindings:bind(<<"*.execute.put.news">>, ?MODULE, handle_put),
    _ = crossbar_bindings:bind(<<"*.execute.post.news">>, ?MODULE, handle_post),
    _ = crossbar_bindings:bind(<<"*.execute.delete.news">>, ?MODULE, handle_delete).

-define(PATH_VERIFY, <<"verify">>).
-define(PATH_MEMBER, <<"members">>).
-define(PATH_TYPE, <<"types">>).

allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].

allowed_methods(_Id) -> [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

allowed_methods(_Id,?PATH_VERIFY) -> [?HTTP_POST];

allowed_methods(_Id,?PATH_MEMBER) -> [?HTTP_POST, ?HTTP_DELETE].

-spec resource_exists() -> true.
resource_exists() -> true.

-spec resource_exists(path_token()) -> true.
resource_exists(_Id) -> true.

-spec resource_exists(path_token(),path_token()) -> true.
resource_exists(_Id,?PATH_VERIFY) ->true;
resource_exists(_Id,?PATH_MEMBER) ->true.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate_verb(Context, cb_context:req_verb(Context)).

authenticate_verb(Context, ?HTTP_GET) -> true;

authenticate_verb(Context, _) -> 
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, Path) ->
    authenticate_verb(Context, Path,cb_context:req_verb(Context)).

authenticate_verb(Context, _, ?HTTP_GET) -> true;
authenticate_verb(Context, _, _) -> 
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).


-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(Context, _Id, _) ->
    Token = cb_context:auth_token(Context),
    app_util:oauth2_authentic(Token, Context).

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize_verb(Context, cb_context:req_verb(Context)).

authorize_verb(Context, ?HTTP_GET) -> true;

authorize_verb(Context, ?HTTP_PUT) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_USER_GE).

-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, Path) ->
    authorize_verb(Context, Path, cb_context:req_verb(Context)).

authorize_verb(Context, Path, ?HTTP_GET) -> true;

authorize_verb(Context, Path, ?HTTP_POST) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_USER_GE);

authorize_verb(Context, Path, ?HTTP_DELETE) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_USER_GE).

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Id, ?PATH_VERIFY) ->
    Role = cb_context:role(Context),
    authorize_util:check_role(Role, ?USER_ROLE_OPERATOR_GE);

authorize(_Context, _Id, ?PATH_MEMBER) -> true.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).

validate(Context, Id, Path) ->
    validate_request(Id, Path, Context, cb_context:req_verb(Context)).

%%%%%%%%%%%%%%%%
%%  HANDLERS  %%
%%%%%%%%%%%%%%%%

-spec handle_get(req_ctx()) -> req_ctx().
handle_get({Req, Context}) ->
    QueryJson = cb_context:query_string(Context),
    Limit = zt_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
    Offset = zt_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
    PropQueryJson = wh_json:to_proplist(QueryJson),
    News = news_db:find_by_conditions([], PropQueryJson, Limit, Offset),
    PropNews = 
        lists:map(fun (Info) ->
            get_sub_fields(Info)
        end,News),
    {Req,
     cb_context:setters(Context,
                        [{fun cb_context:set_resp_data/2, PropNews},
                         {fun cb_context:set_resp_status/2, success}])}.

-spec handle_get(req_ctx(), path_token()) -> req_ctx().
handle_get({Req, Context}, ?PATH_TYPE) ->
    Types = [],
    
    {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, Types},
                               {fun cb_context:set_resp_status/2, success}])};

handle_get({Req, Context}, Id) ->
    case news_db:find(Id) of
      #{} = Info ->
          PropNews = get_sub_fields(Info),
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_data/2, PropNews},
                               {fun cb_context:set_resp_status/2, success}])};
      _ ->
          {Req,
           cb_context:setters(Context,
                              [{fun cb_context:set_resp_error_msg/2, <<"News not found">>},
                               {fun cb_context:set_resp_status/2, <<"error">>},
                               {fun cb_context:set_resp_error_code/2, 404}])}
    end.

-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
    ReqJson = cb_context:req_json(Context),
    Uuid = zt_util:get_uuid(),    
    UserId = cb_context:user_id(Context),
    case  get_target_type(
                wh_json:get_value(<<"target_type">>, ReqJson, <<>>),
                wh_json:get_value(<<"target_id">>, ReqJson, <<>>)
         ) of 
         {error, ErrorMsg} -> 
                cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, ErrorMsg},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 400}]
            );
         {TargetType, TargetId} -> 
                #{
                    first_name := FirstName, 
                    last_name := LastName
                } = user_db:find(UserId),
                Info = #{
                    id => <<"news", Uuid/binary>>,
                    subject => wh_json:get_value(<<"subject">>, ReqJson,<<>>),
                    content => wh_json:get_value(<<"content">>, ReqJson,<<>>),
                    target_type => TargetType,
                    target_id => TargetId,
                    status => <<"active">>,
                    medias => zt_util:to_map_list(wh_json:get_value(<<"medias">>, ReqJson,[])),
                    published_by_name => <<FirstName/binary," ",LastName/binary>>,
                    published_time => zt_util:now_to_utc_binary(os:timestamp()),
                    published_by_id => UserId,
                    created_by_name => <<FirstName/binary," ",LastName/binary>>,
                    created_time => zt_util:now_to_utc_binary(os:timestamp()),
                    created_by_id => UserId
                },
                news_db:save(Info),
                cb_context:setters(Context,
                                [{fun cb_context:set_resp_data/2, Info},
                                    {fun cb_context:set_resp_status/2, success}])
    end.
    
    

get_target_type(<<>>,_) -> {error,target_type_required};

get_target_type(_,<<>>) -> {error,target_id_required};

get_target_type(<<"category">> = TargetType,CategoryId) -> 
    case configuration_db:find(CategoryId) of 
    #{
        group := <<"news_category">>,
        type := <<"system">>
     } -> 
        {TargetType, CategoryId};
    _ -> 
        {error,notfound_category}    
    end;

get_target_type(<<"group">> = TargetType,GroupId) -> 
    case group_db:find(GroupId) of 
    notfound -> 
        {error,notfound_group};
    #{} -> 
        {TargetType, GroupId}
    end;

get_target_type(<<"sos_request">> = TargetType, Id) -> 
    case sos_request_db:find(Id) of 
    notfound -> 
        {error,notfound_request};
    #{} -> 
        {TargetType, Id}
    end;
get_target_type(OtherType,OtherId) -> 
lager:debug("OtherType: ~p,OtherId: ~p~n",[OtherType,OtherId]),
{error,target_required}.

-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, Id) ->
    CustomerId = cb_context:user_id(Context),
    case news_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"News Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        #{
            subject := SubjectDb,
            detail_info := DetailInfoDb,
            content := ContentDb,
            medias := MediasDb
        } = InfoDb -> 
         ReqJson =  cb_context:req_json(Context),

        NewDetailInfo = 
            case wh_json:get_value(<<"detail_info">>, ReqJson, <<>>) of
                <<>> ->  DetailInfoDb;
                DetailInfoProps -> zt_util:to_map(DetailInfoProps)
            end,

         #{
             first_name := FirstName, 
             last_name := LastName
            } = user_db:find(CustomerId),

            NewMedias = 
            case wh_json:get_value(<<"medias">>, ReqJson, []) of
                [] ->  MediasDb;
                MediaProps -> zt_util:to_map_list(MediaProps)
            end,

         NewInfo = 
            maps:merge(InfoDb, #{
                subject => wh_json:get_value(<<"subject">>, ReqJson,SubjectDb),
                description => wh_json:get_value(<<"content">>, ReqJson,ContentDb), 
                detail_info => NewDetailInfo,
                medias => NewMedias,
                updated_name => <<FirstName/binary," ",LastName/binary>>,
                updated_time => zt_util:now_to_utc_binary(os:timestamp()),
                updated_by_id => CustomerId
            }),
         news_db:save(NewInfo),
         cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                             ])            
    end.

-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, Id,?PATH_VERIFY) ->
    case news_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"News Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        InfoDb -> 
         ReqJson =  cb_context:req_json(Context),
         UserId =  cb_context:user_id(Context),
         #{
            email := Email,
            first_name := FirstName,
            last_name := LastName
         } = user_db:find(UserId),
         NewInfo = 
            maps:merge(InfoDb, #{
                verify_status => wh_json:get_value(<<"verify_status">>, ReqJson),
                verify_info => #{
                    user_id => UserId,
                    email => Email,
                    first_name => FirstName,
                    last_name => LastName,
                    verify_description => wh_json:get_value(<<"verify_description">>, ReqJson,<<>>),
                    time => zt_util:now_to_utc_binary(os:timestamp())
                }
            }),
         news_db:save(NewInfo),
         cb_context:setters(Context
                            ,[{fun cb_context:set_resp_data/2, NewInfo}
                              ,{fun cb_context:set_resp_status/2, 'success'}
                             ])            
    end.

-spec handle_delete(cb_context:context(), path_token()) -> cb_context:context().
handle_delete(Context, Id) ->
    case news_db:find(Id) of 
        notfound -> 
            cb_context:setters(Context,
                [{fun cb_context:set_resp_error_msg/2, <<"News Not Found">>},
                {fun cb_context:set_resp_status/2, 'error'},
                {fun cb_context:set_resp_error_code/2, 404}]
            );
        _ -> 
            
            news_db:del_by_id(Id),
            Resp = #{
                id => Id
            },
            cb_context:setters(Context,
                       [{fun cb_context:set_resp_data/2, Resp},
                        {fun cb_context:set_resp_status/2, success}])
    end.

permissions() ->
  authorize_util:default_permission(?MODULE).
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_request(Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(Context, ?HTTP_PUT = Verb) ->
    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun validate_subject/2,
                    fun validate_content/2
                   % fun validate_detail_info/2
                ],
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(Context, _Verb) ->
    Context.

validate_request(_Id, Context, ?HTTP_GET) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, ?HTTP_POST = _Verb) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);
validate_request(_Id, Context, _Verb) ->
    Context.

validate_request(_Id, ?PATH_VERIFY, Context, ?HTTP_POST) ->

    ReqJson = cb_context:req_json(Context),
    Context1 = cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]),
    ValidateFuns = [
                    fun validate_verify_status/2],
                   
    lists:foldl(fun (F, C) ->
                        F(ReqJson, C)
                end,
                Context1,
                ValidateFuns);

validate_request(_Id, ?PATH_MEMBER, Context, _) ->
    cb_context:setters(Context, [{fun cb_context:set_resp_status/2, success}]);

validate_request(_Id, _Path, Context, _Verb) ->
    Context.

get_sub_fields(Group) ->
    Res = maps:to_list(Group),
    proplists:substitute_aliases([], Res).

validate_subject(ReqJson, Context) ->
    Type = wh_json:get_value(<<"subject">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"subject">>, Type).

validate_content(ReqJson, Context) ->
    Subject = wh_json:get_value(<<"content">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"content">>, Subject).

validate_detail_info(ReqJson, Context) ->
    Content = wh_json:get_value(<<"detail_info">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"detail_info">>, Content).
validate_verify_status(ReqJson, Context) ->
    Type = wh_json:get_value(<<"verify_status">>, ReqJson, <<>>),
    api_util:check_val(Context, <<"verify_status">>, Type).