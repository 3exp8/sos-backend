-module(province_handler).

-include("crossbar.hrl").
-include("app.hrl").

-export([
    find_district_by_code/2,
    find_Ward_by_code/2,
    get_province_district_ward_info/3,
    get_address_detail_info/1
]).

find_district_by_code([], _Code) -> notfound;
find_district_by_code([#{<<"code">> := Code} = Info|DistrictList], Code) -> Info;

find_district_by_code([Info|WardList], Code) -> 
    lager:debug("find_Ward_by_code ~p~n",[Info]),
    find_district_by_code(WardList,Code).
        
find_Ward_by_code([], _Code) -> notfound;
find_Ward_by_code([#{<<"code">> := Code} = Info|WardList], Code) -> Info;

find_Ward_by_code([Info|WardList], Code) -> 
    lager:debug("find_Ward_by_code ~p~n",[Info]),
    find_Ward_by_code(WardList,Code).


get_address_detail_info(AddressProps) when is_list(AddressProps) -> 
    get_address_detail_info(zt_util:to_map(AddressProps));

get_address_detail_info(ReqAddressInfoRaw) -> 
    ReqAddressInfo = zt_util:map_keys_to_atom(ReqAddressInfoRaw),
    ReqAddressInfoDb = 
        get_province_district_ward_info(
            maps:get(province_id, ReqAddressInfo,<<>>),
            maps:get(district_code, ReqAddressInfo, 0),
            maps:get(ward_code, ReqAddressInfo, 0)
        ),
     maps:merge(ReqAddressInfoDb, #{
        address => maps:get(address, ReqAddressInfo,<<>>)
    }).

get_province_district_ward_info(<<>>, _DistrictCodeStr, _WardCodeStr) -> #{};

get_province_district_ward_info(ProvinceId, DistrictCodeStr, WardCodeStr) -> 
    case province_db:find(ProvinceId) of 
        notfound -> #{};
        #{
            name := ProvinceName,
            districts := Districts 
        } -> 
            ProvinceInfo = #{
                province_id => ProvinceId,
                province_name => ProvinceName
            },
            DistrictCode = zt_util:to_integer(DistrictCodeStr),
            case find_district_by_code(Districts,DistrictCode) of 
                notfound -> ProvinceInfo;
                #{
                    <<"name">> := DistrictName,
                    <<"wards">> := Wards
                } -> 
                    DistrictInfo = maps:merge(ProvinceInfo, #{
                        district_code => DistrictCode,
                        district_name => DistrictName
                    }), 
                    WardCode = zt_util:to_integer(WardCodeStr),
                    case find_district_by_code(Wards,WardCode) of 
                        notfound -> DistrictInfo;
                        #{
                            <<"name">> := WardName
                        } -> 
                            maps:merge(DistrictInfo, #{
                                ward_code => WardCode,
                                ward_name => WardName
                            })
                    end
            end
    end.