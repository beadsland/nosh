-ifndef(package).
-module(test).
-package(default).
-else.
-module(?package.test).
-package(?package).
-endif.

-hello(world).

-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 