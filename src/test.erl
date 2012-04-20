-module(test).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	io:format("Hello!~n",[]),
	{ok, self()}.

stop(_State) ->
	ok.