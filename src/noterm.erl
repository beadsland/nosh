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

%% @doc Terminal emulator for `nosh'.
%% Translates standard I/O to Erlang messaging.
%%
%% <i>Full terminal emulation has yet to be implemented.</i>
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: io:get_char (see code in jungerl)
%% TODO: escript and parameters (make it run like any other shell command)
%% TODO: make fully redistributable (Win/cygwin/*NIX)
%% TODO: incorporate full terminfo/ncurses support
%% TODO: notermd - telent/ssh access

%% @version 0.1.5
-module(noterm).
-version("0.1.5").

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-include("macro.hrl").

%%
%% Exported functions
%%

-export([start/0,start_wecho/0]).

% Private Exports
-export([key_start/1]).
-export([msg_loop/1, key_loop/1]).

%%
%% API functions
%%

%% @doc Start terminal, launching message loop and keyboard listening
%% process.
%% @end
start() -> start(false).

%% @doc Start terminal, with nosh echo flag set.
%%
%% This is a stopgap measure pending proper terminal emulation.
%% @end
start_wecho() -> io:format("Shell echo flag enabled.\n"), start(true).

%%
%% Local functions
%%

start(Echo) ->
  error_logger:tty(false),
  IO = ?IO(self()),     % hack pending proper pose functions
  ENV = ?ENV,
  ?INIT_POSE,
  io:format("Starting Noterm ~s terminal emulator on ~p ~p~n",
        [?VERSION(?MODULE), node(), self()]),

  KeyPid = spawn_link(?MODULE, key_start, [self()]),

  try spawn_link(nosh, run, [?IO(self(), self(), self(), Echo)]) of
    NoshPid             -> msg_loop(?IO(KeyPid, NoshPid, NoshPid))
  catch
    {Message, Reason}   -> grace(Message, Reason), init:stop()
  end.


%%@private Export to allow for hotswap.
msg_loop(IO) ->
  receive
    {purging, _Pid, _Mod}		-> ?MODULE:msg_loop(IO);
    {'EXIT', ExitPid, Reason}	-> do_exit(IO, ExitPid, Reason);
    {MsgTag, Stdin, Line}
      when Stdin == IO#std.in 	-> do_keyin(IO, MsgTag, Line);
    {MsgTag, Stdout, Line}
      when Stdout == IO#std.out	-> do_noshout(IO, MsgTag, Line);
    {MsgTag, Stderr, Line}
      when Stderr == IO#std.err	-> do_noshout(IO, MsgTag, Line);
    Noise						-> do_noise(IO, Noise)
  end.

% Handle nosh process messages.
do_noshout(IO, MsgTag, Output) ->
  case MsgTag of
    stdout	-> io:format("~s", [Output]);
    erlout	-> io:format("~p: data: ~p~n", [nosh, Output]);
    erlerr	-> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr	-> io:format(standard_error, "** ~s", [Output]);
    debug	-> io:format(standard_error, "-- ~s", [Output])
  end,
  ?MODULE:msg_loop(IO).

% Handle keyboard process messages.
do_keyin(IO, MsgTag, Line) ->
  Clean = strip_escapes(Line),
  case MsgTag of
    stdout  -> ?STDOUT(Clean);
    stderr	-> io:format(standard_error, "** ~s", [Line]);
    debug   -> io:format(standard_error, "-- ~s", [Line])
  end,
  ?MODULE:msg_loop(IO).

% Handle exit signals.
do_exit(IO, ExitPid, Reason) ->
  case ExitPid of
    Stdin when Stdin == IO#std.in		->
      ?DEBUG("Stopping shell on keyboard exit", [Reason]),
      ?STDOUT("stop\n"),
      ?MODULE:msg_loop(IO);
    Stdout when Stdout == IO#std.out	->
      grace("Stopping terminal on shell exit", Reason),
      init:stop()
  end.

% Handle message queue noise.
do_noise(IO, Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]),
  ?MODULE:msg_loop(IO).

grace(Message, Reason) ->
  io:format(standard_error, "~s: ~s~n", [Message, ?FORMAT_ERLERR(Reason)]).

strip_escapes(Subject) ->
  {ok, MP} = re:compile("\e\[[\d,\s]+[A-Z]"),
  re:replace(Subject, MP, "", [global, {return, list}]).

%%========================================
%% Functions for keyboard process.
%%========================================

%%@private Export to allow for spawn.
key_start(Pid) ->
  IO = ?IO(Pid),
  ENV = ?ENV,
  ?INIT_POSE,
  ?DEBUG("Listening to keyboard ~p~n", [self()]),
  key_loop(IO).

%%@private Export to allow for hotswap.
key_loop(IO) ->
  case io:get_line("") of
    ok			    -> key_receive(IO);
    eof 			-> key_stop(eof);
    ".\n"			-> key_stop(eof);
    {error, Reason} -> ?STDERR("error: ~p~n", [Reason]);
    Line			-> ?STDOUT(Line)
  end,
  ?MODULE:key_loop(IO).

key_receive(IO) ->
  receive
    {purging, _Pid, _Mod}		-> true; % chase your tail
    {'EXIT', Stdin, Reason}
      when Stdin == IO#std.in	-> io:format("~p exit: ~p~n",
                                             [?MODULE, Reason]);
    Noise						-> ?STDERR("noise: ~p ~p~n",
                                           [Noise, self()])
  after
    1 -> false
  end.

key_stop(Reason) ->
  ?DEBUG("Stopping keyboard: ~p~n", [Reason]), exit(Reason).