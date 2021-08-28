%%%-------------------------------------------------------------------
%% @doc wh_mqtt_hanler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wh_mqtt_hanler_sup).
-include_lib("whistle/include/wh_types.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([start_child/0
				 , remove_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILDREN, []).

start_child() ->
	supervisor:start_child(?MODULE, ?CHILDREN).

remove_child(Connection) when is_pid(Connection) ->
	supervisor:terminate_child(?MODULE, Connection),
	supervisor:delete_child(?MODULE, Connection).


start_link() ->
	%cb_wh_mqtt_execute:start_link().
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
	SupFlags = #{strategy => one_for_all,
							 intensity => 0,
							 period => 1},
	ChildSpecs = [],
	{ok, {SupFlags, ?CHILDREN}}.

%% internal functions
