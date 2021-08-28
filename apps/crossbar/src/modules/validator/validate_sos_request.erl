-module(validate_sos_request).

-export([send_sos_request/2]).

-export([validate_address_info/3, validate_contact_info/3]).

send_sos_request(ReqJson, Context) ->
        Settings = [
                %{not_null, <<"subject">>}
                 %   ,{not_null, <<"description">>}
                 %   ,{not_null, <<"support_types">>}
                    {{one_of, [<<"guest">>, <<"user">>, <<"group">>]}, <<"requester_type">>}
                  %  ,{object, <<"address_info">>}
                    ,{object, <<"contact_info">>}
                   ],
        lists:foldl(fun({T, Key}, C) ->
                                    KeyValue = wh_json:get_value(Key, ReqJson, <<>>),
                                    app_util:validate_key(?MODULE, {T, Key, Key}, Key, KeyValue, C)
                    end, Context, Settings
                   ).

validate_address_info(Prefix, ReqJson, Context) ->
        Settings = [{not_null, <<"address">>},
                    {not_null, <<"town">>},
                    {not_null, <<"ward">>},
                    {not_null, <<"district">>}
                   ],
        lists:foldl(fun({T, Key}, C) ->
                                    KeyValue = wh_json:get_value(Key, ReqJson, <<>>),
                                    KN = app_util:key_name(Prefix, Key),
                                    app_util:validate_key(?MODULE, {T, Key, KN}, KN, KeyValue, C)
                    end, Context, Settings).

validate_contact_info(Prefix, ReqJson, Context) ->
        Settings = [{not_null, <<"name">>},
                    {not_null, <<"phone_number">>}
                   ],
        lists:foldl(fun({T, Key}, C) ->
                                    KeyValue = wh_json:get_value(Key, ReqJson, <<>>),
                                    KN = app_util:key_name(Prefix, Key),
                                    app_util:validate_key(?MODULE, {T, Key, KN}, KN, KeyValue, C)
                    end, Context, Settings).

