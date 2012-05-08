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

%% @version 0.1.10
-module(nosh).
-version("0.1.10").

%%
%% Include files
%%

%-define(debug, true).
-include("macro.hrl").

% This will eventually draw from ENV...
-define(PROMPT, ?STDOUT("nosh> ")).

%%
%% Exported functions
%%

-export([start/1]).

% private exports
-export([loop/3,bang_run/2,command_run/2,hotswap_run/2]).

%%
%% API functions
%%

%% @doc Start nosh, receiving standard I/O from noterm.

start(Pid) when is_pid(Pid) -> start(?IO(Pid, Pid, Pid));
start(IO) ->
	error_logger:tty(false),
	process_flag(trap_exit, true),
	?INIT_DEBUG, 
	?STDOUT("Starting Nosh ~s nosql shell ~p~n", [?VERSION(?MODULE), 
												  self()]),
	?DEBUG("Using rev. ~s command line parser~n", [?VERSION(nosh_parse)]),
	?DEBUG("Using rev. ~s module loader~n", [?VERSION(nosh_load)]),
	
	nosh_load:test(IO),

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
								   Stdin == IO#std.in ->
			do_line(IO, Line);		
		{MsgTag, CmdPid, Payload} 					->
			do_output(IO, Cmd, CmdPid, MsgTag, Payload);
		Noise when CmdPid /= self() 				-> 
			do_noise(IO, Cmd, CmdPid, Noise)
	end.

% Handle messages from executing command.
do_output(IO, Command, CmdPid, MsgTag, Output) ->
	case MsgTag of
		erlout	-> ?STDOUT("~s: ~p~n", [Command, Output]);
		erlerr	-> Erlerr = nosh_util:format_erlerr(Output),
                   ?STDERR("~s: ~s~n", [Command, Erlerr]);
		stdout	-> ?STDOUT(Output);
		stderr 	-> ?STDERR(Output);
		debug 	-> IO#std.err ! {debug, self(), Output}
	end,
	?MODULE:loop(IO, Command, CmdPid).

%% Handle next command line to execute.
%% @todo refactor `hot' and `good' as library commands
%% @todo refactor bang commands as direct invocations
do_line(IO, Line) ->
	case Line of
		[$! | BangCmd] ->
			BangPid = spawn_link(nosh, bang_run, [?IO(self()), BangCmd]),
			?MODULE:loop(IO, bang, BangPid);
		"hot\n"	->
			HotPid = spawn_link(nosh, hotswap_run, [?IO(self()), "hot\n"]),
			?MODULE:loop(IO, hot, HotPid);
		"good\n" ->
			code:add_path("deps/superl/ebin"),
			GoodPid = spawn_link(superl, start, []),
			?MODULE:loop(IO, good, GoodPid);		
		_Line ->
			NewPid = spawn_link(nosh, command_run, [?IO(self()), Line]),
            NewCom = string:tokens(Line, "\n"),
			?MODULE:loop(IO, NewCom, NewPid)
	end.

% Handle termination of processes.
do_exit(IO, Command, CmdPid, ExitPid, Reason) ->
	if ExitPid == IO#std.in ->
		   ?DEBUG("Stopping on terminal exit: ~p ~p~n", [Reason, self()]),
		   init:stop();
	   ExitPid == CmdPid	->
		   command_return(IO, Command, Reason),
		   ?PROMPT,
		   ?MODULE:loop(IO, ?MODULE, self());	   
	   Reason == normal 	->
		   ?DEBUG("Saw process exit: ~p~n", [ExitPid]),
		   ?MODULE:loop(IO, Command, CmdPid);
	   true 				->
		   ?STDERR("Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
		   init:stop()
	end.
		   
% Handle noise on message queue.
do_noise(IO, Command, CmdPid, Noise) ->
	?STDERR("noise: ~p ~p~n", [Noise, self()]),
	?MODULE:loop(IO, Command, CmdPid).

% We spawn command as separate process and then wait on it.
% This allows us to catch the exit status of runtime errors.

command_return(IO, Command, Status) -> 
	case Status of
		normal -> 
			?DEBUG("~s: ~p~n", [Command, Status]);
		
		ok -> 
			?DEBUG("~s: ~p~n", [Command, Status]);
		
		{ok, Result} -> 
			?STDOUT("~s: ok: ~p~n", [Command, Result]);
		
		{error, {Type, Message}} ->
			?STDERR("nosh: ~p error: ~s~n", [Type, Message]);
		
		{{Except, Detail}, Trace} -> 
			Format = "~s: ~p~nDetail: ~p~nTrace: ~p~n",
			?STDERR(Format, [Command, Except, Detail, Trace]);

		{Error, [Source | Trace]} ->
			Format = "~s: runtime error: ~p~nSource: ~p~nTrace: ~p~n",
			?STDERR(Format, [Command, Error, Source, Trace]);

		Else -> 
			?STDERR("~s: ~p~n", [Command, Else])
	end.

% spawned as process.
bang_run(IO, BangCmd) ->
	Chop = string:strip(BangCmd, right, $\n),
	Output = os:cmd(Chop),
	if is_atom(Output)	-> exit(Output);
       Output == []		-> exit(ok);
       true				-> ?STDOUT("~s: ~s", [Chop, Output]),
						   exit(ok)
    end.

% spawned as a process
command_run(IO, Line) ->
	Parse = nosh_parse:parse(IO, Line),
	exit(Parse).

%%%%
% Development hotswapping.  This should be refactored as a command.
%%%%
hotswap_run(IO, _Line) ->
	?STDOUT("Hotswapping nosh modules\n"), 
	hotswap(IO, noterm),
	hotswap(IO, nosh),
	hotswap_nosh(IO, code:all_loaded()),
	exit(ok).

hotswap_nosh(_IO, []) -> ok;
hotswap_nosh(IO, [{Module, _Path} | Tail]) ->
	{ok, MP} = re:compile("^nosh_"),
	case re:run(atom_to_list(Module), MP, [{capture, none}]) of
		match	->	?DEBUG("see ~p~n", [Module]),
					hotswap(IO, Module);
		nomatch	->	true
	end,
	hotswap_nosh(IO, Tail).

% @todo refactor this given new nosh_load implementation
hotswap(IO, Module) ->
	try
		nosh_load:run(IO, Module)
	catch
		{Error, Detail}	-> 
			?STDERR("~p: ~p~nDetail: ~p~n", [Module, Error, Detail]) 
    end.