-module(app_ctl).
-export([start/0
		, process/1]).

-include("app_commands.hrl").

-define(B(Arg), list_to_binary(B)).
-define(U(Arg), list_to_binary(Arg)).

-spec start() -> none().
start() ->
	% io:format("start \n"),
	case init:get_plain_arguments() of
	[SNode | Args0]  = Params ->
		% io:format("start: Params: ~p \n",[Params]),
		Args = args_join_xml(args_join_strings(Args0)),
		SNode1 = case string:tokens(SNode, "@") of
			[_Node, _Server] ->
			 SNode;
			_ ->
			 case net_kernel:longnames() of
				 true ->
					 SNode ++ "@" ++ inet_db:gethostname() ++
						 "." ++ inet_db:res_option(domain);
				 false ->
					 SNode ++ "@" ++ inet_db:gethostname();
				 _ ->
					 SNode
			 end
			end,
		Node = list_to_atom(SNode1),
		% io:format("Node: ~p \n; ~p \n",[Node, Args]),
		Status = case rpc:call(Node, ?MODULE, process, [Args]) of
			{badrpc, Reason} ->
			io:format("Failed RPC connection to the node ~p: ~p~n",
					[Node, Reason]),
			 %% TODO: show minimal start help
			 ?STATUS_BADRPC;
			S -> S
		end,
		halt(Status);
	_ ->
		print_usage(),
		halt(?STATUS_USAGE)
	end.


process([]) ->
	process(["help"]);

process(["help" | Mode]) ->
    {MaxC, ShCode} = get_shell_info(),
    case Mode of
	[] ->
		print_default_usage(),
	    print_usage(dual, MaxC, ShCode),
	    ?STATUS_USAGE;
	["--dual"] ->
		print_default_usage(),
	    print_usage(dual, MaxC, ShCode),
	    ?STATUS_USAGE;
	["--long"] ->
		print_default_usage(),
	    print_usage(long, MaxC, ShCode),
	    ?STATUS_USAGE;
	["help"] ->
		print_default_usage(),
	    print_usage_help(MaxC, ShCode),
	    ?STATUS_SUCCESS;
	[CmdString | _] ->
	    CmdStringU = re:replace(CmdString, "-", "_", [global, {return, list}]),
	    print_usage_commands(CmdStringU, MaxC, ShCode),
	    ?STATUS_SUCCESS
    end;

process(Args) ->
	{String, Code} = process2(Args),
	case String of
		[] -> ok;
		_ ->
			io:format("~s~n", [String])
	end,
	Code.



process2(Args) ->
	case try_call_command(Args) of
		{String, wrong_command_arguments}
		when is_list(String) ->
			io:format(lists:flatten(["\n" | String]++["\n"])),
			[CommandString | _] = Args,
			process(["help" | [CommandString]]),
			{lists:flatten(String), ?STATUS_ERROR};
		{String, Code}
		when is_list(String) and is_integer(Code) ->
			{lists:flatten(String), Code};
		String
		when is_list(String) ->
			{lists:flatten(String), ?STATUS_SUCCESS};
		Code
		when is_integer(Code) ->
			{"", Code};
		Other ->
			{"Erroneous result: " ++ io_lib:format("~p", [Other]), ?STATUS_ERROR}
	end.


try_call_command(Args) ->
	try call_command(Args) of
		{error, command_unknown} ->
			{io_lib:format("Error: command ~p not known.", [hd(Args)]), ?STATUS_ERROR};
		{error, wrong_number_parameters} ->
			{"Error: wrong number of parameters", ?STATUS_ERROR};
		Res ->
			Res
	catch
		A:Why ->
			Stack = erlang:get_stacktrace(),
			{io_lib:format("Problem '~p ~p' occurred executing the command.~nStacktrace: ~p", [A, Why, Stack]), ?STATUS_ERROR}
	end.

  
call_command([CmdString | Args]) ->
	CmdStringU = re:replace(CmdString, "-", "_", [global, {return, list}]),
	Command = list_to_atom(CmdStringU),
	case app_handle_commands:get_command_format(Command) of
	{error, command_unknown} ->
		{error, command_unknown};
	{ArgsFormat, ResultFormat} ->
		case (catch format_args(Args, ArgsFormat)) of
		ArgsFormatted when is_list(ArgsFormatted) ->
			Result = app_handle_commands:execute_command(Command,
								   ArgsFormatted),
			format_result(Result, ResultFormat);
		{'EXIT', {function_clause,[{lists,zip,[A1, A2], _FileInfo} | _]}} ->
			{NumCompa, TextCompa} =
			case {length(A1), length(A2)} of
				{L1, L2} when L1 < L2 -> {L2-L1, "less argument"};
				{L1, L2} when L1 > L2 -> {L1-L2, "more argument"}
			end,
			{io_lib:format("Error: the command ~p requires ~p ~s.",
				   [CmdString, NumCompa, TextCompa]),
			 wrong_command_arguments}
		end
	end.


print_usage_help(MaxC, ShCode) ->
    LongDesc =
        [
         "Please note that 'sos help' shows all commands,\n",
         "even those that cannot be used in the shell with sosctl.\n",
         "Those commands can be identified because the description starts with: *"],
    ArgsDef = [],
    C = #app_commands{
      desc = "Show help of commands",
      longdesc = LongDesc,
      args = ArgsDef,
      result = {help, string}},
    print_usage_command("help", C, MaxC, ShCode).


args_join_strings([]) ->
    [];
args_join_strings([ "\"", NextArg | RArgs ]) ->
    args_join_strings([ "\"" ++ NextArg | RArgs ]);
args_join_strings([ [ $" | _ ] = Arg | RArgs ]) ->
    case lists:nthtail(length(Arg)-2, Arg) of
        [C1, $"] when C1 /= $\ ->
            [ string:substr(Arg, 2, length(Arg)-2) | args_join_strings(RArgs) ];
        _ ->
            [NextArg | RArgs1] = RArgs,
            args_join_strings([Arg ++ " " ++ NextArg | RArgs1])
    end;
args_join_strings([ Arg | RArgs ]) ->
    [ Arg | args_join_strings(RArgs) ].


args_join_xml([]) ->
    [];
args_join_xml([ [ $< | _ ] = Arg | RArgs ]) ->
    case bal(Arg, $<, $>) of
        true ->
            [Arg | args_join_xml(RArgs)];
        false ->
            [NextArg | RArgs1] = RArgs,
            args_join_xml([Arg ++ " " ++ NextArg | RArgs1])
    end;
args_join_xml([ Arg | RArgs ]) ->
    [ Arg | args_join_xml(RArgs) ].

print_usage() ->
    {MaxC, ShCode} = get_shell_info(),
    print_usage(dual, MaxC, ShCode).
print_usage(HelpMode, MaxC, ShCode) ->
    AllCommands = get_list_commands(),
    io:format(
       ["Usage: ", "sos ", "[--node ", ?U("nodename"), "] ", ?U("command"), "\n", 
	"Available command in this app node:\n"], []),
    print_usage_commands(HelpMode, MaxC, ShCode, AllCommands).


print_default_usage() ->
	io:format([" \n Usage: sos ", "{start|start_boot <file>|foreground|stop|restart|", 
		"reboot|ping|console|getpid|console_clean|console_boot <file>|attach|debug|remote_console|upgrade} \n\n"], []).

filter_commands(All, SubString) ->
    case lists:member(SubString, All) of
        true -> [SubString];
        false -> filter_commands_regexp(All, SubString)
    end.
filter_commands_regexp(All, Glob) ->
    RegExp = xmerl_regexp:sh_to_awk(Glob),
    lists:filter(
      fun(Command) ->
              case re:run(Command, RegExp, [{capture, none}]) of
              match ->
                  true;
              nomatch ->
                  false
              end
      end,
      All).

print_usage_commands(HelpMode, MaxC, ShCode, Commands) ->
    CmdDescsSorted = lists:keysort(1, Commands),
    %% What is the length of the largest command?
    {CmdArgsLenDescsSorted, Lens} =
	lists:mapfoldl(
	  fun({Cmd, Args, Desc}, Lengths) ->
		  Len =
		      length(Cmd) +
		      lists:foldl(fun(Arg, R) ->
					  R + 1 + length(Arg)
				  end,
				  0,
				  Args),
		  {{Cmd, Args, Len, Desc}, [Len | Lengths]}
	  end,
	  [],
	  CmdDescsSorted),
    MaxCmdLen = case Lens of
		    [] -> 80;
		    _ -> lists:max(Lens)
		end,
    %% For each command in the list of commands
    %% Convert its definition to a line
    FmtCmdDescs = format_command_lines(CmdArgsLenDescsSorted, MaxCmdLen, MaxC, ShCode, HelpMode),
    io:format([FmtCmdDescs], []).


print_usage_commands(CmdSubString, MaxC, ShCode) ->
    %% Get which command names match this substring
    AllCommandsNames = [atom_to_list(Name) || {Name, _, _} <- app_handle_commands:list_commands()],
    Cmds = filter_commands(AllCommandsNames, CmdSubString),
    case Cmds of
        [] -> io:format("Error: not command found that match: ~p~n", [CmdSubString]);
        _ -> print_usage_commands2(lists:sort(Cmds), MaxC, ShCode)
    end.

print_usage_commands2(Cmds, MaxC, ShCode) ->
    %% Then for each one print it
    lists:mapfoldl(
      fun(Cmd, Remaining) ->
              print_usage_command(Cmd, MaxC, ShCode),
              case Remaining > 1 of
                  true -> io:format([" ", lists:duplicate(MaxC, 126), " \n"], []);
                  false -> ok
              end,
              {ok, Remaining-1}
      end,
      length(Cmds),
      Cmds).

print_usage_command(Cmd, MaxC, ShCode) ->
    Name = list_to_atom(Cmd),
    case app_handle_commands:get_command_definition(Name) of
        command_not_found ->
            io:format("Error: command ~p not known.~n", [Cmd]);
        C ->
            print_usage_command(Cmd, C, MaxC, ShCode)
    end.
print_usage_command(Cmd, C, MaxC, _ShCode) ->
    #app_commands{
                     tags = TagsAtoms,
                     desc = Desc,
                     longdesc = LongDesc,
                     args = ArgsDef,
                     result = ResultDef} = C,
    NameFmt = ["  ", "Command Name", ": ", Cmd, "\n"],
    %% Initial indentation of result is 13 = length("  Arguments: ")
    Args = [format_usage_ctype(ArgDef, 13) || ArgDef <- ArgsDef],
    ArgsMargin = lists:duplicate(13, $\s),
    ArgsListFmt = case Args of
                      [] -> "\n";
                      _ -> [ [Arg, "\n", ArgsMargin] || Arg <- Args]
                  end,
    ArgsFmt = ["  ", "Arguments", ": ", ArgsListFmt],
    %% Initial indentation of result is 11 = length("  Returns: ")
    ResultFmt = format_usage_ctype(ResultDef, 11),
    ReturnsFmt = ["  ","Returns",": ", ResultFmt],
    XmlrpcFmt = "", %%+++ ["  ",?B("XML-RPC"),": ", format_usage_xmlrpc(ArgsDef, ResultDef), "\n\n"],
    TagsFmt = ["  ","Tags",": ", prepare_long_line(8, MaxC, [atom_to_list(TagA) || TagA <- TagsAtoms])],
    DescFmt = ["  ","Description",": ", prepare_description(15, MaxC, Desc)],
    LongDescFmt = case LongDesc of
                      "" -> "";
                      _ -> ["", prepare_description(0, MaxC, LongDesc), "\n\n"]
                  end,
    NoteAppctl = case is_supported_args(ArgsDef) of
                          true -> "";
                          false -> ["  ", "Note:", " This command cannot be executed.\n\n"]
                      end,
    io:format(["\n", NameFmt, "\n", ArgsFmt, "\n", ReturnsFmt, "\n\n", XmlrpcFmt, TagsFmt, "\n\n", DescFmt, "\n\n", LongDescFmt, NoteAppctl], []).


get_shell_info() ->
    %% This function was introduced in OTP R12B-0
    try io:columns() of
	{ok, C} -> {C-2, true};
	{error, enotsup} -> {78, false}
    catch
	_:_ -> {78, false}
    end.


get_list_commands() ->
    try  app_handle_commands:list_commands() of
	Commands ->
	    [tuple_command_help(Command)
	     || {N,_,_}=Command <- Commands,
		N /= status, N /= stop, N /= restart]
    catch
	exit:_ ->
	    []
    end.

tuple_command_help({Name, Args, Desc}) ->
    Arguments = [atom_to_list(ArgN) || {ArgN, _ArgF} <- Args],
    Prepend = case is_supported_args(Args) of
                  true -> "";
                  false -> "*"
              end,
    CallString = atom_to_list(Name),
    {CallString, Arguments, Prepend ++ Desc}.

is_supported_args(Args) ->
    lists:all(
      fun({_Name, Format}) ->
              (Format == integer)
                  or (Format == string)
      end,
      Args).



 format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, dual)
  when MaxC - MaxCmdLen < 40 ->
    %% If the space available for descriptions is too narrow, enforce long help mode
    format_command_lines(CALD, MaxCmdLen, MaxC, ShCode, long);
format_command_lines(CALD, MaxCmdLen, MaxC, _ShCode, dual) ->
    lists:map(
      fun({Cmd, Args, CmdArgsL, Desc}) ->
              DescFmt = prepare_description(MaxCmdLen+4, MaxC, Desc),
              ["  ", Cmd, " ", [[?U(Arg), " "] || Arg <- Args], string:chars($\s, MaxCmdLen - CmdArgsL + 1),
               DescFmt, "\n"]
      end, CALD);

format_command_lines(CALD, _MaxCmdLen, MaxC, _ShCode, long) ->
    lists:map(
      fun({Cmd, Args, _CmdArgsL, Desc}) ->
              DescFmt = prepare_description(8, MaxC, Desc),
              ["\n  ", Cmd, " ", [[?U(Arg), " "] || Arg <- Args], "\n", "        ",
               DescFmt, "\n"]
      end, CALD).

format_usage_ctype(Type, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==rescode) or (Type==restuple) or (Type==binary)->
    io_lib:format("~p", [Type]);
format_usage_ctype({Name, Type}, _Indentation)
  when (Type==atom) or (Type==integer) or (Type==string) or (Type==rescode) or (Type==restuple) or (Type==binary)->
    io_lib:format("~p::~p", [Name, Type]);
format_usage_ctype({Name, {list, ElementDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementFmt = format_usage_ctype(ElementDef, Indentation2),
    [NameFmt, "::[ ", ElementFmt, " ]"];
format_usage_ctype({Name, {tuple, ElementsDef}}, Indentation) ->
    NameFmt = atom_to_list(Name),
    Indentation2 = Indentation + length(NameFmt) + 4,
    ElementsFmt = format_usage_tuple(ElementsDef, Indentation2),
    [NameFmt, "::{ " | ElementsFmt].


format_usage_tuple([], _Indentation) ->
    [];
format_usage_tuple([ElementDef], Indentation) ->
    [format_usage_ctype(ElementDef, Indentation) , " }"];
format_usage_tuple([ElementDef | ElementsDef], Indentation) ->
    ElementFmt = format_usage_ctype(ElementDef, Indentation),
    MarginString = lists:duplicate(Indentation, $\s), % Put spaces
    [ElementFmt, ",\n", MarginString, format_usage_tuple(ElementsDef, Indentation)].


prepare_description(DescInit, MaxC, Desc) ->
    Words = string:tokens(Desc, " "),
    prepare_long_line(DescInit, MaxC, Words).

prepare_long_line(DescInit, MaxC, Words) ->
    MaxSegmentLen = MaxC - DescInit,
    MarginString = lists:duplicate(DescInit, $\s), % Put spaces
    [FirstSegment | MoreSegments] = split_desc_segments(MaxSegmentLen, Words),
    MoreSegmentsMixed = mix_desc_segments(MarginString, MoreSegments),
    [FirstSegment | MoreSegmentsMixed].

split_desc_segments(MaxL, Words) ->
    join(MaxL, Words).

mix_desc_segments(MarginString, Segments) ->
    [["\n", MarginString, Segment] || Segment <- Segments].

join(L, Words) ->
    join(L, Words, 0, [], []).

join(_L, [], _LenLastSeg, LastSeg, ResSeg) ->
    ResSeg2 = [lists:reverse(LastSeg) | ResSeg],
    lists:reverse(ResSeg2);
join(L, [Word | Words], LenLastSeg, LastSeg, ResSeg) ->
    LWord = length(Word),
    case LWord + LenLastSeg < L of
        true ->
            %% This word fits in the last segment
            %% If this word ends with "\n", reset column counter
            case string:str(Word, "\n") of
                0 ->
                    join(L, Words, LenLastSeg+LWord+1, [" ", Word | LastSeg], ResSeg);
                _ ->
                    join(L, Words, LWord+1, [" ", Word | LastSeg], ResSeg)
            end;
        false ->
            join(L, Words, LWord, [" ", Word], [lists:reverse(LastSeg) | ResSeg])
    end.


bal(String, Left, Right) ->
    bal(String, Left, Right, 0).

bal([], _Left, _Right, Bal) ->
    Bal == 0;
bal([$\ , _NextChar | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal);
bal([Left | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal-1);
bal([Right | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal+1);
bal([_C | T], Left, Right, Bal) ->
    bal(T, Left, Right, Bal).


format_args(Args, ArgsFormat) ->
    lists:foldl(
      fun({{_ArgName, ArgFormat}, Arg}, Res) ->
              Formatted = format_arg(Arg, ArgFormat),
              Res ++ [Formatted]
      end,
      [],
      lists:zip(ArgsFormat, Args)).




format_arg(Arg, integer) ->
    format_arg2(Arg, "~d");
format_arg("", string) ->
    "";
format_arg(Arg, string) ->
    NumChars = integer_to_list(string:len(Arg)),
    Parse = "~" ++ NumChars ++ "c",
    format_arg2(Arg, Parse);

format_arg(Arg, binary) ->
    list_to_binary(format_arg(Arg, string));

format_arg(Arg, {list, Type}) ->
    [format_arg(Token, Type) || Token <- string:tokens(Arg, ";")].

-spec format_arg2(Arg :: string(),
                  Parse :: nonempty_string()
                  ) -> [[any()] | char()] | char().
format_arg2(Arg, Parse)->
    {ok, [Arg2], _RemainingArguments} = io_lib:fread(Parse, Arg),
    Arg2.


%%-----------------------------
%% Format result
%%-----------------------------
-spec format_result(In :: tuple() | atom() | integer() | string() | binary(),
                    {_, 'atom'|'integer'|'string'|'binary'}
                    ) -> string() | {string(), _}.
format_result({error, ErrorAtom}, _) ->
    {io_lib:format("Error: ~p", [ErrorAtom]), make_status(error)};
format_result(Atom, {_Name, atom}) ->
    io_lib:format("~p", [Atom]);
format_result(Int, {_Name, integer}) ->
    io_lib:format("~p", [Int]);
format_result(String, {_Name, string}) ->
    io_lib:format("~s", [String]);
format_result(Binary, {_Name, binary}) ->
    io_lib:format("~s", [Binary]);
format_result(Code, {_Name, rescode}) ->
    make_status(Code);
format_result({Code, Text}, {_Name, restuple}) ->
    {io_lib:format("~s", [Text]), make_status(Code)};
%% The result is a list of something: [something()]
format_result([], {_Name, {list, _ElementsDef}}) ->
    "";
format_result([FirstElement | Elements], {_Name, {list, ElementsDef}}) ->
    %% Start formatting the first element
    [format_result(FirstElement, ElementsDef) |
     %% If there are more elements, put always first a newline character
     lists:map(
       fun(Element) ->
               ["\n" | format_result(Element, ElementsDef)]
       end,
       Elements)];
%% The result is a tuple with several elements: {something1(), something2(),...}
%% NOTE: the elements in the tuple are separated with tabular characters,
%% if a string is empty, it will be difficult to notice in the shell,
%% maybe a different separation character should be used, like ;;?
format_result(ElementsTuple, {_Name, {tuple, ElementsDef}}) ->
    ElementsList = tuple_to_list(ElementsTuple),
    [{FirstE, FirstD} | ElementsAndDef] = lists:zip(ElementsList, ElementsDef),
    [format_result(FirstE, FirstD) |
     lists:map(
       fun({Element, ElementDef}) ->
               ["\t" | format_result(Element, ElementDef)]
       end,
       ElementsAndDef)].
-spec make_status(ok | true | _) -> 0 | 1.
make_status(ok) -> ?STATUS_SUCCESS;
make_status(true) -> ?STATUS_SUCCESS;
make_status(_Error) -> ?STATUS_ERROR.