-module(pool_api).

-compile(export_all).


-spec start_link(atom(), list()) -> {ok, pid()}.
start_link(Module, Opts) ->
	PoolSize = proplists:get_value(pool_size, Opts, 100),
    PoolOPts = [
			{workers, PoolSize},
			{worker, { Module, [ undefined, Opts ]}}
			], 
	wpool:start_pool(pool_name(Module), PoolOPts).


-spec start(atom(), list()) -> {ok, pid()}.
start(Module, Opts) ->
    PoolSize = proplists:get_value(pool_size, Opts, 100),
    ChildMods = [ Module, app_pool],
    PoolOPts = [
			{workers, PoolSize},
			{worker, { Module, [ undefined, Opts ]}}
			], 
    supervisor:start_child(crossbar_sup,
			{supervisor_name(Module),
				{ wpool, start_pool, [ pool_name(Module), PoolOPts]}, 
				transient, 2000, supervisor, [ wpool | ChildMods]}).


-spec supervisor_name(atom()) -> atom().
supervisor_name(Module) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ "_sup").

-spec pool_name(atom())-> atom().
pool_name(Module)->
	Module.
    % list_to_atom(atom_to_list(Module) ++ "_pool").


wcast(Module, Message) ->
	wpool:cast(pool_name(Module), Message, next_worker).

wcall(Module, Message) ->
    wpool:call(pool_name(Module), Message, next_worker, 5000).

-spec reply(any(), tuple(), any()) -> {noreply, any()}.
reply(Message, From, State)->
  gen_server:reply(From, Message),
  {noreply, State}.


statistic(Module) ->
	Get = fun proplists:get_value/2,
	InitStats = wpool:stats(pool_name(Module)),
	PoolPid = Get(supervisor, InitStats),
	Options = Get(options, InitStats),
	InitWorkers = Get(workers, InitStats),
	WorkerStatus = 
	[begin
	    WorkerStats = Get(I, InitWorkers),
	    MsgQueueLen = Get(message_queue_len, WorkerStats),
	    Memory = Get(memory, WorkerStats),
	    {status, WorkerStats, MsgQueueLen, Memory}
   	end || I <- lists:seq(1, length(InitWorkers))],
   	[PoolPid, Options, WorkerStatus].


-spec report_overrun(term()) -> ok.
report_overrun(Report) ->
  lager:error("~p", [Report]).
