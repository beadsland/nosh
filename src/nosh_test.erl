%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Test macros.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.4
-module(nosh_test).
-version("0.1.4").

%%
%% Include files
%%

-include("pose/include/interface.hrl").

-include("macro.hrl").

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).

%%
%% Exported Functions
%%

-export([test/1]).

%%
%% API Functions
%%

%% Test that we can throw appropriate warnings in various scenarios.
%% @deprecated
test(IO) ->
  ?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]),

  Root = filename:absname(""),

  test(IO, "test", ?FILENAME(Root, "ebin/alt")),
  test(IO, "test", ?FILENAME(Root, "ebin/alt2")),
  test(IO, "test", ?FILENAME(Root, "ebin")),
  test:start(),

  ?DEBUG("test: done\n").

%%
%% Local Functions
%%

test(IO, Command, Filename) ->
  case pose_code:load(Command, [Filename]) of
    {module, Module, diff_path} -> ?STDERR({Module, "namespace collision"}),
                                   Module:start();
    {module, Module, flat_pkg}  -> ?STDERR({Module, "flat package unsafe"}),
                                   Module:start();
    {module, Module}            -> Module:start();
    {error, What}               -> ?STDERR({test, What})
  end.