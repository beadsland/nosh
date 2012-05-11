-module(test).

-define(debug, true).
-include("pose/include/interface.hrl").

-export([start/0]).

start() -> ?DEBUG("Hello, world!  My first name is ~p.~n", [?MODULE]).