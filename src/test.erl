% BEGIN NOSH PACKAGE PATTERN
-ifndef(package).
-module(test).
-package(default).
-else.
-module(?package.test).
-package(?package).
-endif.
% END NOSH PACKAGE PATTERN

-define(debug, true).
-include("macro.hrl").

-export([start/0]). 

start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]). 