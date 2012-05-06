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

%% @version 0.1.3
-module(noterm).
-version("0.1.3").

%%
%% Include files
%%

-include("macro.hrl").

%%
%% Exported functions
%%

-export([start/0]).

% Private Exports
-export([key_start/1]).
-export([msg_loop/3, key_loop/3]).

%%
%% API functions
%%

%% @doc Start terminal, launching message loop and keyboard listening 
%% process.
%% @end
start() ->
	process_flag(trap_exit, true),
	io:format("Starting Noterm ~s terminal emulator on ~p ~p~n", 
			  [?VERSION(?MODULE), node(), self()]),

	KeyPid = spawn_link(?MODULE, key_start, [self()]),

	try spawn_link(nosh, start, [self()]) of
		NoshPid 			-> msg_loop(KeyPid, NoshPid, NoshPid)
	catch
		{Message, Reason}	-> grace(Message, Reason), init:stop()
	end.

%%
%% Local functions
%%

%%@private Export to allow for hotswap.
msg_loop(Stdin, Stdout, Stderr) ->
	receive
		{purging, _Pid, _Mod}		-> % chase your tail
			?MODULE:msg_loop(Stdin, Stdout, Stderr);
		{'EXIT', ExitPid, Reason}	-> 
			do_exit(Stdin, Stdout, Stderr, ExitPid, Reason);
		{MsgTag, Stdin, Line}		-> 
			do_keyin(Stdin, Stdout, Stderr, MsgTag, Line);
		{MsgTag, Stdout, Line}		-> 
			do_noshout(Stdin, Stdout, Stderr, MsgTag, Line);
		{MsgTag, Stderr, Line}		->
			do_noshout(Stdin, Stdout, Stderr, MsgTag, Line);
		Noise						-> 
			do_noise(Stdin, Stdout, Stderr, Noise)
    end.

% Handle nosh process messages.
do_noshout(Stdin, Stdout, Stderr, MsgTag, Line) ->
	case MsgTag of
		stdout	-> io:format(Line, []);
		stderr	-> io:format(standard_error, "** ~s", [Line]);
		debug	-> io:format(standard_error, "-- ~s", [Line])
	end,
	?MODULE:msg_loop(Stdin, Stdout, Stderr).  

% Handle keyboard process messages.
do_keyin(Stdin, Stdout, Stderr, MsgTag, Line) ->
	case MsgTag of
		stdout	-> ?STDOUT(strip_escapes(Line));
		stderr	-> io:format(standard_error, "** ~s", [Line])
	end,
	?MODULE:msg_loop(Stdin, Stdout, Stderr).  
	
% Handle exit signals.
do_exit(Stdin, Stdout, _Stderr, ExitPid, Reason) ->
	case ExitPid of
		Stdin	-> grace("Stopping on keyboard exit", Reason),
				   exit(normal);
		Stdout	-> grace("Stopping on shell exit", Reason),
				   init:stop();
		_Other	-> Message = io_lib:format("Stopping on ~p exit", [ExitPid]),
				   grace(Message, Reason),
				   exit(normal)
	end.

% Handle message queue noise.
do_noise(Stdin, Stdout, Stderr, Noise) ->
	io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]),
	?MODULE:msg_loop(Stdin, Stdout, Stderr).  
	
grace(Message, Reason) -> 
	case Reason of
		{{Exception, ExcReason}, Trace} 	-> 
			Format = "~s: ~p ~p~nReason: ~p~nTrace: ~p~n",
			io:format(standard_error, Format, 
					  [Message, Exception, self(), ExcReason, Trace]);
		Else						->
			io:format(standard_error, "~s: ~p ~p~n", 
					  [Message, Else, self()])
	end.

strip_escapes(Subject) ->
	{ok, MP} = re:compile("\e\[[\d,\s]+[A-Z]"),
	re:replace(Subject, MP, "", [global, {return, list}]).

%%========================================
%% Functions for keyboard process.
%%========================================

%%@private Export to allow for spawn.
key_start(Pid) ->
	?INIT_DEBUG(Pid),
	?DEBUG("Listening to keyboard ~p~n", [self()]),
	key_loop(Pid, Pid, Pid). 

%%@private Export to allow for hotswap.
key_loop(Stdin, Stdout, Stderr) ->
	case io:get_line("") of 
		ok			    -> key_receive(Stdin, Stdout, Stderr);
		eof 			-> key_stop(eof); 
		".\n"			-> key_stop(eof);
		{error, Reason} -> ?STDERR("error: ~p~n", [Reason]);
		Line			-> ?STDOUT(Line)
	end, 
	?MODULE:key_loop(Stdin, Stdout, Stderr).

key_receive(Stdin, _Stdout, Stderr) ->
	receive
		{purging, _Pid, _Mod}		-> true; % chase your tail
		{'EXIT', Stdin, Reason} 	-> io:format("~p exit: ~p~n", 
												 [?MODULE, Reason]);
		Noise						-> ?STDERR("noise: ~p ~p~n", 
											   [Noise, self()])
	after 
		1 -> false		
	end.

key_stop(Reason) ->
	?DEBUG("Stopping: ~p ~p~n", [Reason, self()]),
    exit(Reason).