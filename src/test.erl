% @TODO figure out if we can pass in package name by some method

-ifndef(package).
-module(test).
-package(default).
-else.
-module(nosh.test).
-package(nosh).
-endif.

-ifdef(test).
-test(?test).
-endif.

-hello(world).

-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 