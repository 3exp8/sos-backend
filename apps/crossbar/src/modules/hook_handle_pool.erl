-module(hook_handle_pool).

-behaviour(gen_server).

-export([start/0, start_link/0]).

-export([
          init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2, 
          code_change/3
        ]).

-export([
          test/1, 
          statistic/0
        ]).

-include("crossbar.hrl").

-spec start_link() -> startlink_ret().
start_link() ->
    % supervisor:start_link({'local', ?MODULE}, ?MODULE, []). 
    pool_api:start_link(?MODULE, []).

start() ->
  pool_api:start(?MODULE, []). 

init([_Var, _Opts]) ->
  {ok, #{}}.


handle_call(Var , _From, State) ->
	lager:info("Other ~p ~n", [Var]).

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason,  _State) ->
  lager:error("Reason terminate ~p ~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

test(Msg) ->
  pool_api:wcast(?MODULE, {test, Msg}).

statistic() ->
  pool_api:statistic(?MODULE).