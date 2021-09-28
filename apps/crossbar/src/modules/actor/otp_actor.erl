-module(otp_actor).

-behaviour(gen_server).

-export([start_link/1,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2]).

-export([
        start_actor/1,
        check_otp_rule/1,
        add_resend/1,
        get_state/1,
        get_pid_name/1
    ]).
    
-define(AUTO_DESTROY_DURATION,30*60*1000). %10 minutes

start_link(PhoneNumber) ->
    Name = get_pid_name(PhoneNumber),
    gen_server:start_link({global, Name}, ?MODULE, [PhoneNumber], []).

init([PhoneNumber]) ->
    erlang:send_after(?AUTO_DESTROY_DURATION, self(), auto_destroyed_timer),
    State = #{
        task_name => get_pid_name(PhoneNumber),
        send_otp_count => 1,
        send_otp_time => zt_datetime:get_now(),
        phone_number => PhoneNumber
    },
    {ok, State}.


handle_call(check_otp_rule, _From, #{
    send_otp_count := Count,
    send_otp_time := LastTime
} = State) ->
    OtpMinInterval = zt_util:to_integer(application:get_env(crossbar, send_otp_min_interval, 60)),
    OtpMaxResend = zt_util:to_integer(application:get_env(crossbar, send_otp_max_resend, 5)),
    Result =
    if 
        Count < OtpMaxResend ->
            Second = zt_datetime:diff_second(LastTime),
            if
                Second < OtpMinInterval -> resend_too_fast;
                true -> true
            end;
        true -> max_resend_reached
    end,
    {reply,Result, State};

handle_call(get_state, _From, State) ->
    {reply,State, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(add_resend, #{
    send_otp_count := Count
} = State) ->
    if
        Count > 3 -> 
            {stop,normal,State};
        true ->
            NewState = maps:merge(State, 
                #{
                    send_otp_count => Count + 1,
                    send_otp_time => zt_datetime:get_now()
                }),
            {noreply, NewState}
    end;

handle_cast(reset, State) ->
    NewState = maps:merge(State, #{
        send_otp_count => 1,
        send_otp_time => zt_datetime:get_now()
    }),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_destroyed_timer, #{task_name := TaskName} = State) ->
    lager:debug("auto_destroyed_timer for ~p job~n ",[TaskName]),
    {stop,  normal, State};

handle_info(_Msg, State) -> {noreply, State}.

code_change(_Msg, State, _Extra) -> {ok, State}.

terminate(Reason, State) -> 
    lager:error("~p terminating... reason: ~p, State: ~p~n",[?MODULE, Reason, State]),
    ok. 
-spec start_actor(binary()) -> boolean().
start_actor(PhoneNumber) -> 
    lager:info("start_actor: ~p ~n",[PhoneNumber]), 
    case zt_util:pid_global(get_pid_name(PhoneNumber)) of 
    undefined -> 
        start_link(PhoneNumber),
        true;
    Srv -> 
        lager:info("start_actor: pid: ~p ~n",[Srv]), 
        gen_server:call(Srv, check_otp_rule)
    end.

check_otp_rule(PhoneNumber) -> 
    lager:info("check_otp_rule: ~p ~n",[PhoneNumber]), 
    case zt_util:pid_global(get_pid_name(PhoneNumber)) of 
        undefined -> phone_number_notfound;
        Srv -> 
            lager:info("check_otp_rule: pid: ~p ~n",[Srv]), 
            gen_server:call(Srv, check_otp_rule)
    end.

get_state(PhoneNumber) -> 
    lager:info("get_state: ~p ~n",[PhoneNumber]), 
    Srv = zt_util:pid_global(get_pid_name(PhoneNumber)),
    lager:info("get_state: pid: ~p ~n",[Srv]), 
    gen_server:call(Srv, get_state).

add_resend(PhoneNumber) -> 
    lager:info("add_resend: ~p ~n",[PhoneNumber]), 
    Srv = zt_util:pid_global(get_pid_name(PhoneNumber)),
    lager:info("add_resend: pid: ~p ~n",[Srv]), 
    gen_server:cast(Srv, add_resend).

get_pid_name(PhoneNumber) ->
    ModuleName = zt_util:to_bin(?MODULE),
    Pid = <<ModuleName/binary, "_", PhoneNumber/binary>>,
    zt_util:to_atom(Pid).