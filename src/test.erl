% @TODO figure out if we can pass in package name by some method

-ifndef(package).
-module(test).
-package(default).
-else.
-module(?package.test).
-package(?package).
-endif.

-hello(world).

-define(debug, true).
-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 