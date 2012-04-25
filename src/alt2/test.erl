-module(test).

-include("../macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My second name is ~p.~n", [?MODULE]). 