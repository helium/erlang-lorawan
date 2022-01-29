-module(lager).

-export([
	warning/2,
	debug/2,
	info/2]).

warning(Format, Value) ->
	io:format(Format, Value).

debug(Format, Value) ->
	io:format(Format, Value).

info(Format, Value) ->
	io:format(Format, Value).