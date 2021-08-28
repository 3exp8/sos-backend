-module(customer_doc).
-behaviour(sumo_doc).
-export([
  sumo_schema/0, 
  sumo_sleep/1, 
  sumo_wakeup/1
]).

-export([email/1, id/1, phone_number/1, type/1,
         first_name/1, last_name/1, password/1]).

-export([
         create_schema/0,
         delete_schema/0,
         init_data/0,
         init_data/1
        ]).

-opaque address()::
#{
  address_arr => binary(),
  city_arr => binary(),
  zip_code_arr => binary()
 }.

-opaque info() :: map().

-type addresses()  :: [address()].
-type id() :: binary().
-type email() :: binary().
-type phone_number() ::  binary().
-type first_name() :: binary().
-type last_name() :: binary().
-type password() ::  binary().
-type type() ::  binary().

-export_type([info/0, address/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> info().
sumo_wakeup(Doc) ->
  #{
    id =>  maps:get(id, Doc, <<>>),
    email   => maps:get(email_id, Doc, <<>>),
    phone_number => maps:get(phone_number_id, Doc, <<>>),
    first_name => maps:get(first_name, Doc, <<>>),
    last_name => maps:get(last_name, Doc, <<>>),
    gender => maps:get(gender, Doc, <<>>),
    dob => nvl(datetime, maps:get(dob_dt, Doc, <<>>)),
    delivery_addresses => deformat_addrr(maps:get(delivery_addresses1, Doc, [])),
    addresses => deformat_addrr(maps:get(addresses1, Doc, [])),
    password => maps:get(password_id, Doc, <<>>),
    avatar => maps:get(avatar_url_email, Doc, <<>>),
    time_zone => maps:get(time_zone, Doc, <<>>),
    status => maps:get(status_id, Doc, <<>>),
    type => maps:get(type_id, Doc, <<>>),
    referral_code => maps:get(referral_code, Doc, <<>>),
    referred_by => maps:get(referred_by, Doc, <<>>),
    confirm_code =>maps:get(confirm_code_id, Doc, <<>>),
    confirm_code_created_time_dt =>maps:get(confirm_code_created_time_dt, Doc, <<>>),
    created_by => maps:get(created_by_id, Doc, <<>>),
    created_time_dt => maps:get(created_time_dt, Doc, <<>>),
    updated_by => maps:get(updated_by_id, Doc, <<>>),
    updated_time_dt => maps:get(updated_time_dt, Doc, <<>>)
   }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(info()) -> sumo:doc().
sumo_sleep(Customer) ->
  DefaultTime = util_db:now_to_utc_binary({0,0,0}),
  #{
    id =>  maps:get(id, Customer, <<>>),
    email_id   => maps:get(email, Customer, <<>>),
    phone_number_id => maps:get(phone_number, Customer, <<>>),
    first_name => maps:get(first_name, Customer, <<>>),
    last_name => maps:get(last_name, Customer, <<>>),
    gender => maps:get(gender, Customer, <<>>),
    dob_dt => maps:get(dob, Customer, DefaultTime),
    delivery_addresses1 => enformat_addr(maps:get(delivery_addresses, Customer, [])),
    addresses1 => enformat_addr(maps:get(addresses, Customer, [])),
    password_id => maps:get(password, Customer, <<>>),
    avatar_url_email => maps:get(avatar, Customer, <<>>),
    time_zone => maps:get(time_zone, Customer, <<>>),
    status_id => maps:get(status, Customer, <<>>),
    type_id => maps:get(type, Customer, <<>>),
    referral_code => maps:get(referral_code, Customer, <<>>),
    referred_by => maps:get(referred_by, Customer, <<>>),
    confirm_code_id =>maps:get(confirm_code, Customer, <<>>),
    confirm_code_created_time_dt =>maps:get(confirm_code_created_time_dt, Customer, DefaultTime),
    created_by_id => maps:get(created_by, Customer, <<>>),
    created_time_dt => maps:get(created_time_dt, Customer, DefaultTime),
    updated_by_id => maps:get(updated_by, Customer, <<>>),
    updated_time_dt => maps:get(updated_time_dt, Customer, DefaultTime)
   }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
                            sumo:new_field(id, binary, [not_null, id]),
                            sumo:new_field(email_id, binary),
                            sumo:new_field(phone_number_id, binary, [ not_null]),
                            sumo:new_field(first_name, string),
                            sumo:new_field(last_name, string),
                            sumo:new_field(gender, binary),
                            sumo:new_field(dob_dt, datetime),
                            sumo:new_field(delivery_addresses1, object_list), %% TODO: array
                            sumo:new_field(addresses1, object_list), %% TODO: array
                            sumo:new_field(avatar_url_email, string),
                            sumo:new_field(time_zone , string),
                            sumo:new_field(status_id , binary),
                            sumo:new_field(type_id , binary),
                            sumo:new_field(referral_code, binary),
                            sumo:new_field(referred_by, binary),
                            sumo:new_field(confirm_code_id , binary),
                            sumo:new_field(confirm_code_created_time_dt , datetime),
                            sumo:new_field(created_by_id , binary),
                            sumo:new_field(created_time_dt , datetime),
                            sumo:new_field(updated_by_id , binary),
                            sumo:new_field(updated_time_dt , datetime)
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
-spec id(info()) -> id().
id(#{id:= Id}) -> Id.

-spec email(info()) -> email().
email(#{email := Email})  ->
  Email.

-spec phone_number(info()) -> phone_number().
phone_number(#{phone_number := PhoneNumber}) ->
  PhoneNumber.

-spec first_name(info()) -> first_name().
first_name(#{first_name := FirstName}) ->
  FirstName.

-spec last_name(info()) -> last_name().
last_name(#{last_name := LastName}) ->
  LastName.

-spec password(info()) -> password().
password(#{password := Password}) ->
  Password.

-spec type(info()) -> type().
type(#{type := Type})  ->
  Type.


enformat_addr(Addresses) ->
  lists:foldl(fun(Address, Acc) ->
                  AddressProps =  util_db:trans_props(Address),
                  AddressVal = proplists:get_value(<<"address">>, AddressProps, <<>>),
                  CityVal = proplists:get_value(<<"city">>, AddressProps, <<>>),
                  ZipCodeVal = proplists:get_value(<<"zip_code">>, AddressProps, <<>>),
                  if AddressVal /= <<>>; CityVal /= <<>>; ZipCodeVal /= <<>> ->
                       CombineVal = #{address_arr => AddressVal, city_arr => CityVal, zip_code_arr => ZipCodeVal},
                       [CombineVal | Acc];
                     true -> Acc
                  end
              end, [], Addresses).

deformat_addrr(<<>>) -> [];
deformat_addrr(undefined) -> [];
deformat_addrr(Address) when is_map(Address) ->
  deformat_addrr([Address]);
deformat_addrr(Addresses) ->
  lists:flatmap(fun(Address) ->
                    AddressProps = util_db:trans_props(Address),
                    TransRes = proplists:substitute_aliases([{address_arr, <<"address">>},{city_arr, <<"city">>},
                                                             {zip_code_arr, <<"zip_code">>}, {<<"address_arr">>, <<"address">>},{<<"city_arr">>, <<"city">>},
                                                             {<<"zip_code_arr">>, <<"zip_code">>}], AddressProps),
                    [maps:from_list(TransRes)]
                end, Addresses).

nvl(Type, <<>>) ->
  nvl(Type);

nvl(Type, <<"undefined">>) ->
  nvl(Type);

nvl(Type, Val) ->
  Val.

nvl(datetime) ->
  util_db:now_to_utc_binary({0,0,0}).

init_data() ->
  Id = <<"customerddfc2382b892fae1d4857b8cae670e73">>,
  init_data(Id).


init_data(<<>>) ->
  Id = app_util:get_id(<<"customer">>),
  init_data(Id);

init_data(Id) ->
  DefaultTime = util_db:now_to_utc_binary({0,0,0}),
  Doc = #{addresses => [],avatar => <<>>,confirm_code => <<"9330">>,
          confirm_code_created_time_dt => DefaultTime,
          created_by => Id,
          created_time_dt => DefaultTime,
          delivery_addresses => [],
          dob => DefaultTime,
          email => <<>>,
          first_name => <<"Customer">>,
          id => Id,
          last_name => <<"Demo">>,
          password => <<>>,
          phone_number => <<"+0888100418">>,
          referral_code => customer_util:get_referral_code(),
          status => <<"active">>,
          time_zone => <<>>,
          type => <<>>,
          updated_by => Id,
          updated_time_dt => DefaultTime},
  customer_db:save(Doc).