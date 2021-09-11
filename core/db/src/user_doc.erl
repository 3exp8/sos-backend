-module(user_doc).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-export([email/1, id/1, phone_number/1,
        first_name/1, last_name/1, password/1, role/1]).

-export([
            create_schema/0,
            delete_schema/0,
            init_data/0
        ]).

-opaque user() :: map().

-type id() :: binary().
-type email() :: binary().
-type phone_number() ::  binary().
-type first_name() :: binary().
-type last_name() :: binary().
-type  password() ::  binary().
-type  role() :: binary().


-export_type([user/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> user().
sumo_wakeup(Doc) ->
	#{
        id =>  maps:get(id, Doc, <<>>),
        account_id =>  maps:get(account_id, Doc, <<>>),
        email   => maps:get(email_id, Doc, <<>>),
        phone_number => zt_util:nvl(maps:get(phone_number_id, Doc, <<>>)),
        first_name => maps:get(first_name, Doc, <<>>),
        last_name => maps:get(last_name, Doc, <<>>),
        address => maps:get(address, Doc, <<>>),
        password => maps:get(password_id, Doc, <<>>),
        role => maps:get(role_id, Doc, <<>>),
        roles => maps:get(roles, Doc, []),
        avatar => maps:get(avatar_url_email, Doc, <<>>),
        time_zone => maps:get(time_zone, Doc, <<>>),
        created_by => maps:get(created_by_id, Doc, <<>>),
        created_time_dt => maps:get(created_time_dt, Doc, <<>>),
        updated_by => maps:get(updated_by_id, Doc, <<>>),
        updated_time_dt => maps:get(updated_time_dt, Doc, <<>>),
        status => maps:get(status_id, Doc, <<>>),
        confirm_code =>maps:get(confirm_code_id, Doc, <<>>),
        confirm_code_created_time_dt =>maps:get(confirm_code_created_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(user()) -> sumo:doc().
sumo_sleep(User) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, User, <<>>),
        account_id =>  maps:get(account_id, User, <<>>),
        email_id   => maps:get(email, User, <<>>),
        phone_number_id => maps:get(phone_number, User, <<>>),
        first_name => maps:get(first_name, User, <<>>),
        last_name => maps:get(last_name, User, <<>>),
        address => maps:get(address, User, <<>>),
        password_id => maps:get(password, User, <<>>),
        role_id => maps:get(role, User, <<>>),
        roles => maps:get(roles, User, []),
        avatar_url_email => maps:get(avatar, User, <<>>),
        time_zone => maps:get(time_zone, User, <<>>),
        created_by_id => maps:get(created_by, User, <<>>),
        created_time_dt => maps:get(created_time_dt, User, DefaultTime),
        updated_by_id => maps:get(updated_by, User, <<>>),
        updated_time_dt => maps:get(updated_time_dt, User, DefaultTime),
        status_id => maps:get(status, User, <<>>),
        confirm_code_id =>maps:get(confirm_code, User, <<>>),
        confirm_code_created_time_dt =>maps:get(confirm_code_created_time_dt, User, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
        sumo:new_field(id, binary, [not_null, id]),
        sumo:new_field(account_id, binary),
        sumo:new_field(email_id, binary, [ not_null]),
        sumo:new_field(phone_number_id, binary, [ not_null]),
        sumo:new_field(first_name, string),
        sumo:new_field(last_name, string),
        sumo:new_field(address, string),
        sumo:new_field(password_id , binary, [ not_null]),
        sumo:new_field(role_id, binary),
        sumo:new_field(roles, object_list),
        sumo:new_field(avatar_url_email, string),
        sumo:new_field(time_zone, string, [ not_null]),
        sumo:new_field(created_by_id, binary, [ not_null]),
        sumo:new_field(created_time_dt, datetime, [ not_null]),
        sumo:new_field(updated_by_id, binary, [ not_null]),
        sumo:new_field(updated_time_dt, datetime, [ not_null]),
        sumo:new_field(status_id, binary, [ not_null]),
        sumo:new_field(confirm_code_id, binary),
        sumo:new_field(confirm_code_created_time_dt, datetime)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_schema() -> ok.
create_schema() ->
  sumo:create_schema(?MODULE).  
  
 -spec delete_schema() -> ok.
delete_schema() ->
  sumo:delete_schema(?MODULE).    

%% @doc Returns callid of the given user.
-spec id(user()) -> id().
id(#{id:= Id}) -> Id.

-spec email(user()) -> email().
email(#{email := Email})  ->
   Email.

-spec phone_number(user()) -> phone_number().
phone_number(#{phone_number := PhoneNumber}) ->
  PhoneNumber.

-spec first_name(user()) -> first_name().
first_name(#{first_name := FirstName}) ->
  FirstName.

-spec last_name(user()) -> last_name().
last_name(#{last_name := LastName}) ->
  LastName.

-spec password(user()) -> password().
password(#{password := Password}) ->
  Password.

-spec role(user()) -> role().
role(#{role := Role})  ->
   Role.

init_data() ->
U = 
#{account_id => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   address => <<"hcm">>,
   avatar => <<>>,
   confirm_code => <<"56e3">>,
   confirm_code_created_time_dt => <<"2017-11-06T06:16:02Z">>,
   created_by => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   created_time_dt => <<"2017-11-06T06:16:02Z">>,
   email => <<"sos.demo@mailnesia.com">>,
   first_name => <<"SOS">>,
   id => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   last_name => <<"demo">>,
   password => <<"$2a$10$JLQLOnz99Wd9R1D1KKEKpO57Ev/vGnMmj37.xq4SR8CdpQdlJ9U9q">>,
   phone_number => <<"+841111111111">>,
   role => <<"USER">>,
   roles => [#{<<"related_id">> => <<"all">>,
      <<"related_type">> => <<"all">>,
      <<"roles">> => [<<"admin">>]}],
   status => <<"active">>,
   time_zone => <<"Asia/Ho_Chi_Minh">>,
   updated_by => <<"accountddfc2382b892fae1d4857b8cae670e73">>,
   updated_time_dt => <<"2019-08-12T07:49:26Z">>},
   user_db:save(U).