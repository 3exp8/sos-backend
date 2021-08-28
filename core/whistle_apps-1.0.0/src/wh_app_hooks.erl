-module(wh_app_hooks).
%%% module copy from ejabberd process-one
-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 add/3,
	 add/4,
	 add/5,
	 delete/3,
	 delete/4,
	 delete/5,
	 run/2,
	 run/3,
	 run_fold/3,
	 run_fold/4]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).


-ifdef(DEPRECATED_GET_STACKTRACE).
-define(EX_RULE(Class, Reason, Stack), Class:Reason:Stack).
-define(EX_STACK(Stack), Stack).
-else.
-define(EX_RULE(Class, Reason, _), Class:Reason).
-define(EX_STACK(_), erlang:get_stacktrace()).
-endif.


-record(state, {}).
-type hook() :: {Seq :: integer(), Module :: atom(), Function :: atom() | fun()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add(atom(), fun(), integer()) -> ok.
%% @doc See add/4.
add(Hook, Function, Seq) when is_function(Function) ->
    add(Hook, global, undefined, Function, Seq).

-spec add(atom(), HostOrModule :: binary() | atom(), fun() | atom() , integer()) -> ok.
add(Hook, Host, Function, Seq) when is_function(Function) ->
    add(Hook, Host, undefined, Function, Seq);

%% @doc Add a module and function to this hook.
%% The integer sequence is used to sort the calls: low number is called before high number.
add(Hook, Module, Function, Seq) ->
    add(Hook, global, Module, Function, Seq).

-spec add(atom(), binary() | global, atom(), atom() | fun(), integer()) -> ok.
add(Hook, Host, Module, Function, Seq) ->
    gen_server:call(?MODULE, {add, Hook, Host, Module, Function, Seq}).

-spec delete(atom(), fun(), integer()) -> ok.
%% @doc See del/4.
delete(Hook, Function, Seq) when is_function(Function) ->
    delete(Hook, global, undefined, Function, Seq).

-spec delete(atom(), binary() | atom(), atom() | fun(), integer()) -> ok.
delete(Hook, Host, Function, Seq) when is_function(Function) ->
    delete(Hook, Host, undefined, Function, Seq);

%% @doc Delete a module and function from this hook.
%% It is important to indicate exactly the same information than when the call was added.
delete(Hook, Module, Function, Seq) ->
    delete(Hook, global, Module, Function, Seq).

-spec delete(atom(), binary() | global, atom(), atom() | fun(), integer()) -> ok.
delete(Hook, Host, Module, Function, Seq) ->
    gen_server:call(?MODULE, {delete, Hook, Host, Module, Function, Seq}).

-spec run(atom(), list()) -> ok.
%% @doc Run the calls of this hook in order, don't care about function results.
%% If a call returns stop, no more calls are performed.
run(Hook, Args) ->
    run(Hook, global, Args).

-spec run(atom(), binary() | global, list()) -> ok.
run(Hook, Host, Args) ->
    Fun = fun() ->
        try ets:lookup(hooks, {Hook, Host}) of
    	[{_, Ls}] ->
    	    run1(Ls, Hook, Args);
    	[] ->
    	    ok
        catch _:badarg ->
    	    ok
        end
    end,
    spawn(Fun).

-spec run_fold(atom(), T, list()) -> T.
%% @doc Run the calls of this hook in order.
%% The arguments passed to the function are: [Val | Args].
%% The result of a call is used as Val for the next call.
%% If a call returns 'stop', no more calls are performed.
%% If a call returns {stop, NewVal}, no more calls are performed and NewVal is returned.
run_fold(Hook, Val, Args) ->
    run_fold(Hook, global, Val, Args).

-spec run_fold(atom(), binary() | global, T, list()) -> T.
run_fold(Hook, Host, Val, Args) ->
    try ets:lookup(hooks, {Hook, Host}) of
	[{_, Ls}] ->
	    run_fold1(Ls, Hook, Val, Args);
	[] ->
	    Val
    catch _:badarg ->
	    Val
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init([]) ->
    lager:info("wh_app_hooks: init ",[]),
    _ = ets:new(hooks, [named_table, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({add, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_add(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call({delete, Hook, Host, Module, Function, Seq}, _From, State) ->
    HookFormat = {Seq, Module, Function},
    Reply = handle_delete(Hook, Host, HookFormat),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    lager:warning("Unexpected call from ~p: ~p", [From, Request]),
    {noreply, State}.

-spec handle_add(atom(), atom(), hook()) -> ok.
handle_add(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            case lists:member(El, Ls) of
                true ->
                    ok;
                false ->
                    NewLs = lists:merge(Ls, [El]),
                    ets:insert(hooks, {{Hook, Host}, NewLs}),
                    ok
            end;
        [] ->
            NewLs = [El],
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok
    end.

-spec handle_delete(atom(), atom(), hook()) -> ok.
handle_delete(Hook, Host, El) ->
    case ets:lookup(hooks, {Hook, Host}) of
        [{_, Ls}] ->
            NewLs = lists:delete(El, Ls),
            ets:insert(hooks, {{Hook, Host}, NewLs}),
            ok;
        [] ->
            ok
    end.

handle_cast(Msg, State) ->
    lager:warning("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
-spec run1([hook()], atom(), list()) -> ok.
run1([], _Hook, _Args) ->
    ok;
run1([{_Seq, Module, Function} | Ls], Hook, Args) ->
    Res = safe_apply(Hook, Module, Function, Args),
    case Res of
	'EXIT' ->
	    run1(Ls, Hook, Args);
	stop ->
	    ok;
	_ ->
	    run1(Ls, Hook, Args)
    end.

-spec run_fold1([hook()], atom(), T, list()) -> T.
run_fold1([], _Hook, Val, _Args) ->
    Val;
run_fold1([{_Seq, Module, Function} | Ls], Hook, Val, Args) ->
    Res = safe_apply(Hook, Module, Function, [Val | Args]),
    case Res of
	'EXIT' ->
	    run_fold1(Ls, Hook, Val, Args);
	stop ->
	    Val;
	{stop, NewVal} ->
	    NewVal;
	NewVal ->
	    run_fold1(Ls, Hook, NewVal, Args)
    end.

safe_apply(Hook, Module, Function, Args) ->
    lager:info("Running hook ~p: ~p:~p/~B",
       [Hook, Module, Function, length(Args)]),
    try if is_function(Function) ->
        apply(Function, Args);
       true ->
        apply(Module, Function, Args)
    end
    % catch E:R when E /= exit; R /= normal ->
    catch ?EX_RULE(E, R, St) when E /= exit; R /= normal ->
        % Stack = ?EX_STACK(St),
        lager:error("Hook ~p crashed when running ~p:~p/~p:~n"
               "** Reason = ~p~n"
               "** Arguments = ~p",
               [Hook, Module, Function, length(Args),
            {E, R, get_stacktrace()} | Args]),
        'EXIT'
    end.

get_stacktrace() ->
    [{Mod, Fun, Loc, Args} || {Mod, Fun, Args, Loc} <- erlang:get_stacktrace()].