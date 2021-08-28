-module(loglevel).

-export([set/1]).

set(5) ->
	lager:set_loglevel(lager_file_backend, "console.log", debug);
set(4) ->
	lager:set_loglevel(lager_file_backend, "console.log", info);
set(3) ->
	lager:set_loglevel(lager_file_backend, "console.log", error);
set(_) ->
	ok.