-module(notification_doc).

-behaviour(sumo_doc).

-export([
			sumo_schema/0, 
			sumo_sleep/1, 
			sumo_wakeup/1
		]).

-export([
			create_schema/0, 
			delete_schema/0
		]).

-export([
			id/1, 
			read_by/1,
			init_data/0
		]).

-opaque destination() :: 
#{
	type => binary(),
	id => binary()
}.

-opaque read_by_detail() :: 
#{
	name => binary(),
	time => binary(),	
	id => binary(),
	type => binary()
}.

-opaque notification() :: map().

-type read_by() :: [read_by_detail()].

-type id() :: binary().

-export_type([
	notification/0, 
	destination/0, 
	read_by_detail/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> notification().
sumo_wakeup(Doc) ->
	#{
		id => maps:get(id, Doc, <<>>),
		destinations => deformat_destinations(maps:get(destinations, Doc, [])),
		target_type => maps:get(target_type, Doc, <<>>),
		target_id => maps:get(target_id, Doc, <<>>),
		transport => maps:get(transport, Doc, <<"push">>),
		type => maps:get(type, Doc, <<>>),
		detail_results => maps:get(detail_results, Doc, []),
		notification => maps:get(notification, Doc, #{}),
		data => maps:get(data, Doc, #{}),
		description => maps:get(description, Doc, <<>>),
		read_by => deformat_read_by(maps:get(read_by, Doc, [])),
		created_by => maps:get(created_by, Doc, <<>>),
		updated_by => maps:get(updated_by, Doc, <<>>),
		created_time => maps:get(created_time, Doc, <<>>),
		updated_time => maps:get(updated_time, Doc, <<>>)
	}.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(notification()) -> sumo:doc().
sumo_sleep(Notification) ->
	DefaultTime = util_db:now_to_utc_binary({0, 0, 0}),
	#{
		id => maps:get(id, Notification, <<>>),
		destinations => enformat_destinations(maps:get(destinations, Notification, [])),
		target_type => maps:get(target_type, Notification, <<>>),
		target_id => maps:get(target_id, Notification, <<>>),
		transport => maps:get(transport, Notification, <<>>),
		type => maps:get(type, Notification, <<>>),
		detail_results => maps:get(detail_results, Notification, []),
		notification => maps:get(notification, Notification, #{}),
		data => maps:get(data, Notification, #{}),
		description => maps:get(description, Notification, <<>>),
		read_by => enformat_read_by(maps:get(read_by, Notification, [])),
		created_by => maps:get(created_by, Notification, <<>>),
		updated_by => maps:get(updated_by, Notification, <<>>),
		created_time => maps:get(created_time, Notification, DefaultTime),
		updated_time => maps:get(updated_time, Notification, DefaultTime)
	}.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
	sumo:new_schema(?MODULE, [
			sumo:new_field(id, binary, [not_null, id]),
			sumo:new_field(destinations, object_list, [not_null]),
			sumo:new_field(target_type, binary, [not_null]),
			sumo:new_field(target_id, binary),
			sumo:new_field(transport, binary),%push, mqtt
			sumo:new_field(type, binary), % notification, data
			sumo:new_field(detail_results, object_list), 
			sumo:new_field(notification, object),
			sumo:new_field(data, object),

			sumo:new_field(description, string),
			sumo:new_field(read_by, object_list, [not_null]),
			sumo:new_field(created_by, binary, [not_null]),
			sumo:new_field(created_time, datetime, [not_null]),
			sumo:new_field(updated_by, binary),
			sumo:new_field(updated_time, datetime)
		]).

-spec create_schema() -> ok.
create_schema() ->
	sumo:create_schema(?MODULE).

-spec delete_schema() -> ok.
delete_schema() -> 
sumo:delete_schema(?MODULE).

%% @doc Returns call of the given notification
enformat_destinations(Destinations) ->
	lists:foldl(fun(Destination, Acc) ->
				DestinationProps = util_db:trans_props(Destination),
				TransRes = proplists:substitute_aliases([
						{<<"type">>, type}, 
						{<<"id">>, id}
					], DestinationProps),
				DestinationInfo = maps:from_list(TransRes),
				[DestinationInfo | Acc]
	end, [], Destinations).

deformat_destinations(<<>>) -> [];

deformat_destinations(Destination) when is_map(Destination) ->
	deformat_destinations([Destination]);

deformat_destinations(Destinations) ->
	lists:flatmap(fun(Destination) -> 
		DestinationProps = util_db:trans_props(Destination),
		TransRes = proplists:substitute_aliases([
				{<<"type">>, type}, 
				{<<"id">>, id}
			], DestinationProps),
		[maps:from_list(TransRes)]
	end, Destinations).

enformat_read_by(Read_By) -> 
	lists:foldl(fun(Read_By_Detail, Acc) ->
		ReadByDetailProps = util_db:trans_props(Read_By_Detail),
		TransRes = proplists:substitute_aliases([
				
				{<<"id">>, id},
				{<<"type">>, type}, 
				{<<"name">>, name}, 
				{<<"time">>, time}
			], ReadByDetailProps),
		ReadInfo = maps:from_list(TransRes),
				[ReadInfo | Acc]
	end, [], Read_By).

deformat_read_by(<<>>) -> [];
deformat_read_by(Read_By_Detail) when is_map(Read_By_Detail) ->
	deformat_read_by([Read_By_Detail]);
deformat_read_by(Read_By) ->
	lists:flatmap(fun(Read_By_Detail) ->
		ReadByDetailProps = util_db:trans_props(Read_By_Detail),
		TransRes = proplists:substitute_aliases([
				{<<"name">>, name},
				{<<"time">>, time},
				{<<"id">>, id},
				{<<"type">>, type}
			], ReadByDetailProps),
		[maps:from_list(TransRes)]
		%[Read_By_Detail]
	end, Read_By).


init_data() -> 
N = #{
	id => <<"Unknown">>,
	destinations => [
		#{
			<<"type">> => <<"user">>,
			<<"id">> => <<"accountddfc2382b892fae1d4857b8cae670e73">>
		},
		#{
			<<"type">> => <<"customer">>,
			<<"id">> => <<"customer3mi1nol4i1p3ij90h1n2ion2po1ip2pni2">>	
		}
	],
	target_type => <<"Advertise">>,
	target_id => <<"ad01">>,
	icon => <<"/icon.png">>,
	content => <<"This is a content of advertisement notification.">>,
	content_type => <<"text">>,
	image => <<"/image.png">>,
	description => <<"This is a description.">>,
	read_by => [
		#{
			<<"name">> => <<"Tri">>,
			<<"time">> => <<"2017-11-06T06:16:02Z">>,
			<<"id">> => <<"account12faijdalio1i31on4o1i2nfoi1rnonro1">>,
			<<"type">> => <<"user">>
		},
		#{
			<<"name">> => <<"Tuong">>,
			<<"time">> => <<"2018-12-06T06:15:02Z">>,
			<<"id">> => <<"customer3mi1nol4i1p3ij90h1n2ion2po1ip2pni2">>,
			<<"type">> => <<"customer">>
		}
	]
},
notification_db:save(N).


-spec id(notification()) -> id().
id(#{id:= Id}) -> Id.

-spec read_by(notification()) -> read_by().
read_by(#{read_by := Read_By}) -> Read_By.