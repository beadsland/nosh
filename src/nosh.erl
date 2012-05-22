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

%% @doc The `nosh' process module, which handles messaging with a paired
%% `noterm' process.
%%
%% Commands:
%% <dl>
%% <dt>`hot'</dt><dd>hotswap nosh project modules</dd>
%% <dt>`good'</dt><dd>check for superly good nosh code</dd>
%% <dt>`!<i>command</i></dt><dd>pass <i>command</i> to native shell</dd>
%% <dt>`.'</dt><dd>end-of-file (exit nosh application)</dd>
%% </dl>
%%
%% Additionally, `nosh' will include commands from the following
%% subprojects:
%% <ul>
%% <li> <a href="http://github.com/beadsland/nosh_bin">Bourne shell
%%      builtins command set</a> (nosh_bin) </li>
%% <li> <a href="http://github.com/beadsland/nosh_erl">Erl shell
%%      alternative command set</a> (nosh_erl) </li>
%% <li> NoSQL file system command set (nosh_nosql) </li>
%% </ul>
%%
%% <b>Draft Notes</b>
%%
%% % A `nosh' process is launched by first starting {@link noterm}, which
%% handles keyboard input, sending this to the `nosh' process as messages,
%% and presenting output and errors to the user as they are received in
%% message form from the `nosh' process.
%%
%% The `nosh' process continues to run until it receives an end-of-file
%% message from `noterm'.  This is currently produced by typing a period
%% (`.') by itself on a line, followed by a `<newline>'.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.15

-define(module, nosh).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.15").

%%
%% Include files
%%

-define(debug, true).
-include("pose/include/interface.hrl").

-include("macro.hrl").

-import(string).
-import(re).

% This will eventually draw from ENV...
-define(PROMPT, ?STDOUT("nosh> ")).

-import(gen_command).

%%
%% Exported functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Hidden callbacks
-export([do_run/2]).

% private exports
-export([loop/3,command_run/2]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @hidden Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?STDOUT("Starting Nosh ~s nosql shell ~p~n", [?VERSION(?MODULE), self()]),
  ?DEBUG("Using rev. ~s command line parser~n", [?VERSION(nosh_parse)]),
  ?DEBUG("Using rev. ~s module loader~n", [?VERSION(.pose_code)]),
  ?PROMPT,
  ?MODULE:loop(IO, ?MODULE, self()).

%%
%% Local functions
%%

%%@private Export to allow for hotswap.
loop(IO, Cmd, CmdPid) ->
  receive
    {purging, _Pid, _Mod} 						-> % chase your tail
      ?MODULE:loop(IO, Cmd, CmdPid);
    {'EXIT', ExitPid, Reason}					->
      do_exit(IO, Cmd, CmdPid, ExitPid, Reason);
    {stdout, Stdin, Line} when CmdPid == self(),
                            Stdin == IO#std.in  ->
      do_line(IO, Line);
    {MsgTag, CmdPid, Payload} 					->
      do_output(IO, Cmd, CmdPid, MsgTag, Payload);
    Noise when CmdPid == self() 				->
      do_noise(IO, Cmd, CmdPid, Noise)
  end.

% Handle messages from executing command.
do_output(IO, Command, CmdPid, MsgTag, Output) ->
  case MsgTag of
    erlout	-> ?STDOUT("~s: ~p~n", [Command, Output]);
    erlerr	-> ?STDERR("~s: ~s~n", [Command, ?FORMAT_ERLERR(Output)]);
    stdout	-> ?STDOUT(Output);
    stderr 	-> ?STDERR(Output);
    debug 	-> IO#std.err ! {debug, self(), Output}
  end,
  ?MODULE:loop(IO, Command, CmdPid).

%% Handle next command line to execute.
%% @todo refactor `hot' and `good' as library commands
%% @todo refactor bang commands as direct invocations
do_line(IO, Line) ->
  if IO#std.echo -> ?STDOUT(Line); true -> false end,
  case Line of
    "stop\n" -> exit(ok);
    [$! | BangCmd]  ->
      do_loadrun(IO, "bang " ++ BangCmd);
    _Line ->
      case re:run(Line, "\ ", [{capture, none}]) of
        match   -> do_parse(IO, Line);
        nomatch -> do_loadrun(IO, Line)
      end
  end.

% Pass command to load. (We're bypassing parse here.)
do_loadrun(IO, Line) ->
  ?DEBUG("Hack run attempt: ~s", [Line]),
  [Command | Words] = [list_to_atom(X) || X <- string:tokens(Line, " \n")],
  CmdPid = spawn_link(pose, exec, [?IO(self()), ?ARG(Command, Words)]),
  ?DEBUG("Running ~p as ~p~n", [Command, CmdPid]),
  ?MODULE:loop(IO, Command, CmdPid).

%  case pose:spawn(?IO(self()), Command, Words) of
%    {error, Reason} -> ?DEBUG("~s~n", [?FORMAT_ERLERR({hack, Reason})]),
%                       do_parse(IO, Line);
%    CmdPid          -> ?MODULE:loop(IO, Command, CmdPid)
%  end.

% Parse command line.
do_parse(IO, Line) ->
  Command = string:strip(Line, right, $\n),
  ParsePid = spawn_link(nosh, command_run, [?IO(self()), Line]),
  ?MODULE:loop(IO, Command, ParsePid).

% Handle termination of processes.
do_exit(IO, Command, CmdPid, ExitPid, Reason) ->
  if ExitPid == IO#std.in ->
       ?DEBUG("Stopping shell on terminal exit: ~p~n", [Reason]),
       exit(ok);
     ExitPid == CmdPid	->
       ?DEBUG("Saw ~s exit: ~p~n", [Command, ExitPid]),
       command_return(IO, Command, Reason),
       ?PROMPT,
       ?MODULE:loop(IO, ?MODULE, self());
     Reason == normal 	->
       ?DEBUG("Saw process exit: ~p~n", [ExitPid]),
       ?MODULE:loop(IO, Command, CmdPid);
     true 				->
       ?STDERR("Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]),
       exit({exit, {ExitPid, Reason}})
  end.

% Handle noise on message queue.
do_noise(IO, Command, CmdPid, Noise) ->
  ?STDERR("noise: ~p ~p~n", [Noise, self()]),
  ?MODULE:loop(IO, Command, CmdPid).

%% Handle exit message from command process.
command_return(IO, Command, Status) ->
  case Status of
    normal          -> ?DEBUG("~s: ~p~n", [Command, Status]);
    ok              -> ?DEBUG("~s: ~p~n", [Command, Status]);
    {ok, Result}    -> ?STDOUT("~s: ok: ~p~n", [Command, Result]);
    Else            -> ?STDERR("~s: ~p~n", [Command, ?FORMAT_ERLERR(Else)])
  end.

% spawned as a process
command_run(IO, Line) ->
  Parse = nosh_parse:parse(IO, Line),
  exit(Parse).