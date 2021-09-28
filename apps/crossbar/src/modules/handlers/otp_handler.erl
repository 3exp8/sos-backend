-module(otp_handler).

-export([
    check_confirm_code/2,
    get_otp_expired_duration/0,
    otp_valid/3
]).

check_confirm_code(_PhoneNumber, <<>>) -> invalid;
check_confirm_code(<<>>, _) -> invalid;
check_confirm_code(PhoneNumber, ConfirmCode)-> 
    case phone_number_db:find(PhoneNumber) of 
        #{
            confirm_code := ConfirmCode,
            created_time := CreatedTime
        } -> 
            EplasedSeconds = zt_datetime:diff_second(CreatedTime),
            OtpExpiredDuration = get_otp_expired_duration(),
            if 
                EplasedSeconds > OtpExpiredDuration ->  expired;
                true -> true
            end;
        _ -> invalid
    end.

otp_valid(PhoneNumber, PhoneNumber, _) -> true;

otp_valid(_PhoneNumber, <<>>, _) -> true;

otp_valid(_, _NewPhoneNumber, <<>>) -> false;

otp_valid(_, NewPhoneNumber, ConfirmCode) ->
    check_confirm_code(NewPhoneNumber, ConfirmCode).

get_otp_expired_duration() -> 
    zt_util:to_integer(application:get_env(crossbar, otp_expired_duration, 120)).
