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

%% @doc Run underlying OS subshell command as a `nosh' command.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.0
-module(nosh_bang).
-version("0.1.0").

%%
%% Include files
%%

-include("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API export
-export([run/2]).

% hotswap export
-export([loop/3]).

%%
%% API Functions
%%

%% @doc Run an OS command in `nosh' context.
run(IO, Command) -> run(IO, Command, 1000 * 60 * 5).

%%
%% Local Functions
%%

run(IO, Command, Timeout) ->
  ?INIT_POSE,
  Opts = [stderr_to_stdout, exit_status, {line, 500}],
  Port = erlang:open_port({spawn, Command}, Opts),
  if is_port(Port)  -> loop(IO, Port, Timeout);
     true           -> exit(failed_open_port)
  end.

loop(IO, Port, Timeout) ->
  receive
    {Port, {data, {eol, Line}}}     -> ?STDOUT("~s~n", [Line]),
                                       ?MODULE:loop(IO, Port, Timeout);
    {Port, {data, {noeol, Line}}}   -> ?STDOUT("~s", [Line]),
                                       ?MODULE:loop(IO, Port, Timeout);
    {Port, {exit_status, 0}}        -> exit(ok);
    {Port, {exit_status, Status}}   -> exit({status, Status});
    {'EXIT', Port, Reason}          -> exit({bang, Reason});
    Noise                           -> ?STDERR("noise: ~p ~p~n",
                                               [Noise, self()]),
                                       ?MODULE:loop(IO, Port, Timeout)
  after Timeout ->
      exit(timeout)
  end.