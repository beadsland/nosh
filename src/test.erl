% BEGIN NOSH PACKAGE PATTERN
-ifndef(package).
-module(test).
-package(default).
-else.
-module(?package.test).
-package(?package).
-endif.
% END NOSH PACKAGE PATTERN

%%
%% Include files
%%
-define(debug, true).
-include("macro.hrl").

%%
%% Exported Functions
%%
-export([start/0]). 

%%
%% API Functions
%%
start() -> ?DEBUG("Hello, world!  My name is ~p.~n", [?MODULE]).

%%
%% Local Functions
%%