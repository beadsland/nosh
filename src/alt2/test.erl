-module(test).

-define(debug, true).
-include("../pose/include/interface.hrl").

-export([start/0]).

start() -> ?DEBUG("Hello, world!  My second name is ~p.~n", [?MODULE]).