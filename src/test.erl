-ifndef(nosh).
-module(test).
-else.
-module(?nosh.test).
-endif.

-include("macro.hrl").

-export([test/0]). 

test() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 