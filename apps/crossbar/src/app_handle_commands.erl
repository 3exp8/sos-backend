-module(app_handle_commands).

-include("app_commands.hrl").

-export([init/0
		,register_commands/1
		,unregister_commands/1
		,execute_command/2
		,get_command_format/1
		,list_commands/0
		,get_command_definition/1]).


init() ->
	ets:new(app_commands, [named_table, set, public,
								{keypos, #app_commands.name}]).

register_commands(Commands) ->
	lists:foreach(
		fun(Command) ->
			case ets:insert_new(app_commands, Command) of
				true ->
				  ok;
				false ->
				  lager:debug("This command is already defined:~n~p", [Command])
			end
		end,
	  Commands),
	ok.

%% @doc Unregister  commands.
unregister_commands(Commands) ->
	lists:foreach(
	  fun(Command) ->
		ets:delete_object(app_commands, Command)
	  end,
	  Commands),
	ok.


list_commands() ->
	Commands = ets:match(app_commands,
						 #app_commands{name = '$1',
											args = '$2',
											desc = '$3',
											_ = '_'}),
	[{A, B, C} || [A, B, C] <- Commands].

execute_command(Name, Arguments) ->
	Command = get_command_definition(Name),
	execute_check_policy(Command, Arguments).


execute_check_policy(#app_commands{policy = open} = Command , Arguments) ->
	do_execute_command(Command, Arguments);

execute_check_policy(#app_commands{policy = restricted} = Command , Arguments) ->
	do_execute_command(Command, Arguments);

execute_check_policy(_Command, _Arguments) ->
	throw({error, access_rules_unauthorized}).


do_execute_command(Command, Arguments) ->
	Module = Command#app_commands.module,
	Function = Command#app_commands.function,
	lager:debug("Executing command ~p:~p with Args=~p", [Module, Function, Arguments]),
	apply(Module, Function, Arguments).


%% @doc Get the definition record of a command.
get_command_definition(Name) ->
	case ets:lookup(app_commands, Name) of
		[E] -> E;
		[] -> command_not_found
	end.

get_command_format(Name) ->
	Matched = ets:match(app_commands,
						#app_commands{name = Name,
										   args = '$1',
										   result = '$2',
										   _ = '_'}),
	case Matched of
		[] ->
			{error, command_unknown};
		[[Args, Result]] ->
			{Args, Result}
	end.

