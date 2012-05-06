%% -*- erlang -*-

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
%% <dt>`hot'</dt><dd>hotswap nosh modules</dd>
%% <dt>`good'</dt><dd>check for superly good nosh code</dd>
%% <dt>`!<i>command</i></dt><dd>pass <i>command</i> to native shell</dd>
%% <dt>`.'</dt><dd>end-of-file (exit nosh application)</dd>
%% </dl>
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

%% @version 0.1.8
-module(nosh).
-version("0.1.8").

%%
%% Include files
%%

%-define(debug, true).
-include("macro.hrl").

%%
%% Exported functions
%%

-export([start/1]).

% private exports
-export([loop/5,bang_run/4,command_run/4,hotswap_run/4]).

%%
%% API functions
%%

%% @doc Start nosh, passing Pid of process providing standard i/o
%% messaging.
%% @end
start(Pid) -> start(Pid, Pid, Pid).

%%
%% Local functions
%%

start(Stdin, Stdout, Stderr) ->
	error_logger:tty(false),
	process_flag(trap_exit, true),
	?INIT_DEBUG(Stderr), 
	?STDOUT("Starting Nosh ~s nosql shell ~p~n", [?VERSION(?MODULE), 
												  self()]),
	?DEBUG("Using rev. ~s command line parser~n", [?VERSION(nosh_parse)]),
	?DEBUG("Using rev. ~s module loader~n", [?VERSION(nosh_load)]),
	
	nosh_load:test(Stderr),

	prompt(Stdout),
	?MODULE:loop(Stdin, Stdout, Stderr, ?MODULE, self()).

%%@private Export to allow for hotswap.
loop(Stdin, Stdout, Stderr, Cmd, CmdPid) ->
	receive
		{purging, _Pid, _Mod} -> % chase your tail
            ?MODULE:loop(Stdin, Stdout, Stderr, Cmd, CmdPid);
		{'EXIT', ExitPid, Reason}	->
			do_exit(Stdin, Stdout, Stderr, Cmd, CmdPid, ExitPid, Reason);
		{stdout, Stdin, Line} when CmdPid == self()	->
			do_line(Stdin, Stdout, Stderr, Line);			
		{MsgTag, CmdPid, Payload} ->
			do_output(Stdin, Stdout, Stderr, Cmd, CmdPid, MsgTag, Payload);
		Noise when CmdPid /= self() -> 
			do_noise(Stdin, Stdout, Stderr, Cmd, CmdPid, Noise)
	end.

% Handle messages from executing command.
do_output(Stdin, Stdout, Stderr, Command, CmdPid, MsgTag, Output) ->
	case MsgTag of
		stdout	->
			Stdout ! {stdout, self(), Output},
            ?MODULE:loop(Stdin, Stdout, Stderr, Command, CmdPid);
		stderr 	->
			Stderr ! {stderr, self(), Output},
			?MODULE:loop(Stdin, Stdout, Stderr, Command, CmdPid);
		debug 	->
			Stderr ! {debug, self(), Output},
			?MODULE:loop(Stdin, Stdout, Stderr, Command, CmdPid)
	end.

%% Handle next command line to execute.
%% @todo refactor `hot' and `good' as library commands
%% @todo refactor bang commands as direct invocations
do_line(Stdin, Stdout, Stderr, Line) ->
	case Line of
		[$! | BangCmd] ->
			BangPid = spawn_link(nosh, bang_run, 
								 [self(), self(), self(), BangCmd]),
			?MODULE:loop(Stdin, Stdout, Stderr, bang, BangPid);
		"hot\n"	->
			HotPid = spawn_link(nosh, hotswap_run,
							   	[self(), self(), self(), "hot\n"]),
			?MODULE:loop(Stdin, Stdout, Stderr, hot, HotPid);
		"good\n" ->
			code:add_path("deps/superl/ebin"),
			GoodPid = spawn_link(superl, start, []),
			?MODULE:loop(Stdin, Stdout, Stderr, good, GoodPid);		
		_Line ->
			NewPid = spawn_link(nosh, command_run, 
								[self(), self(), self(), Line]),
            NewCom = string:tokens(Line, "\n"),
			?MODULE:loop(Stdin, Stdout, Stderr, NewCom, NewPid)
	end.

% Handle termination of processes.
do_exit(Stdin, Stdout, Stderr, Command, CmdPid, ExitPid, Reason) ->
	if ExitPid == Stdin 	->
		   ?DEBUG("Stopping on terminal exit: ~p ~p~n", [Reason, self()]),
		   init:stop();
	   ExitPid == CmdPid	->
		   command_return(Command, Reason, Stderr),
		   prompt(Stdout),
		   ?MODULE:loop(Stdin, Stdout, Stderr, ?MODULE, self());	   
	   Reason == normal 	->
		   ?DEBUG("Saw process exit: ~p~n", [ExitPid]),
		   ?MODULE:loop(Stdin, Stdout, Stderr, Command, CmdPid);
	   true 				->
		   ?STDERR("Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
		   init:stop()
	end.
		   
% Handle noise on message queue.
do_noise(Stdin, Stdout, Stderr, Command, CmdPid, Noise) ->
	?STDERR("noise: ~p ~p~n", [Noise, self()]),
	?MODULE:loop(Stdin, Stdout, Stderr, Command, CmdPid).

prompt(Stdout) -> 
	Prompt = "nosh> ",
	Stdout ! {stdout, self(), Prompt}.

% We spawn command as separate process and then wait on it.
% This allows us to catch the exit status of runtime errors.

command_return(Command, Status, Stderr) -> 
	case Status of
		normal -> 
			?DEBUG("~s: ~p~n", [Command, Status]);
		
		ok -> 
			?DEBUG("~s: ~p~n", [Command, Status]);
		
		{ok, Result} -> 
			?DEBUG("~s: ~p~n", [Command, Result]);
		
		{error, {Term, Data}} ->
			?STDERR("nosh: ~p~nDetail: ~p~n", [Term, Data]);

		{error, What} ->
			?STDERR("nosh: ~s~n", [What]);
		
		{{Except, Reason}, Trace} -> 
			Format = "~s: ~p~nReason: ~p~nTrace: ~p~n",
			?STDERR(Format, [Command, Except, Reason, Trace]);
		
		Else -> 
			?STDERR("~s: ~p~n", [Command, Else])
	end.

% spawned as process.
bang_run(_Stdin, Stdout, _Stderr, BangCmd) ->
	Chop = string:strip(BangCmd, right, $\n),
	Output = os:cmd(Chop),
	if is_atom(Output)	-> exit(Output);
       Output == []		-> exit(ok);
       true				-> ?STDOUT("~s: ~s", [Chop, Output]),
						   exit(ok)
    end.

% spawned as a process
command_run(_Stdin, _Stdout, Stderr, Line) ->
	Parse = nosh_parse:parse(Line, Stderr),
	exit(Parse).

%%%%
% Development hotswapping.  This should be refactored as a command.
%%%%
hotswap_run(_Stdin, Stdout, Stderr, _Line) ->
	?STDOUT("Hotswapping nosh modules\n"), 
	hotswap(noterm, Stderr),
	hotswap(nosh, Stderr),
	hotswap_nosh(code:all_loaded(), Stderr),
	exit(ok).

hotswap_nosh([], _Stderr) -> ok;
hotswap_nosh([{Module, _Path} | Tail], Stderr) ->
	{ok, MP} = re:compile("^nosh_"),
	case re:run(atom_to_list(Module), MP, [{capture, none}]) of
		match	->	?DEBUG("see ~p~n", [Module]),
					hotswap(Module, Stderr);
		nomatch	->	true
	end,
	hotswap_nosh(Tail, Stderr).

hotswap(Module, Stderr) ->
	{file, Filename} = code:is_loaded(Module),
	try
		nosh_load:load(Module, filename:dirname(Filename), Stderr)
	catch
		{Error, Detail}	-> 
			?STDERR("~p: ~p~nDetail: ~p~n", [Module, Error, Detail]) 
    end.