-ifndef(package).
-module(test).
-else.
-module(?package.test).
-endif.

-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 