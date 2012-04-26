% @TODO figure out if we can pass in package name by some method

-ifndef(package).
-module(test).
-else.
-module(nosh.test).
-endif.

-ifndef(package).
-package(default).
-else.
-package(nosh).
-endif.

-hello(world).

-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 