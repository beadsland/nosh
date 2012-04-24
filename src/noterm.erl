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
%% Translating standard I/O to Erlang messaging.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: io:get_char (see code in jungerl)
%% TODO: escript and parameters (make it run like any other shell command)
%% TODO: make fully redistributable (Win/cygwin/*NIX)
%% TODO: incorporate full terminfo/ncurses support
%% TODO: notermd - telent/ssh access

%% @version 0.1.2
-module(noterm).
-version("0.1.2").

-include("macro.hrl").

-export([start/0]).
-export([key_start/1]).


%% @doc Start terminal, launching message loop and keyboard listening process.
start() ->
	process_flag(trap_exit, true),
	error_logger:tty(false),
	io:format("Starting Noterm ~s terminal emulator on ~p ~p~n", [?VERSION(?MODULE), node(), self()]),
	KeyPid = spawn_link(?MODULE, key_start, [self()]),
	try spawn_link(nosh, start, [self()]) of
		NoshPid 			-> msg_loop(KeyPid, NoshPid, NoshPid)
	catch
		{Message, Reason}	-> grace(Message, Reason), init:stop()
	end.
					   
msg_loop(Stdin, Stdout, Stderr) ->
	receive
		{Stdin, stdout, Line}		-> Stdout ! {self(), stdout, strip_escapes(Line)};
		{Stdin, stderr, Line}		-> io:format(standard_error, "** ~s", [Line]); % key err doesn't go to shell
		{Stdout, stdout, Line} 		-> io:format(Line, []);
		{Stderr, stderr, Line} 		-> io:format(standard_error, "** ~s", [Line]);
		{'EXIT', Stdin, Reason}  	-> grace("Stopping on keyboard exit", Reason), exit(normal);
		{'EXIT', Stdout, Reason}	-> grace("Stopping on shell exit", Reason), init:stop();
		{'EXIT', ExitPid, Reason}	-> grace(io_lib:format("Stopping on ~p exit", [ExitPid]), Reason), exit(normal)
    end,
	msg_loop(Stdin, Stdout, Stderr). 

grace(Message, Reason) -> 
	case Reason of
		{{Exception, ExcReason}, Trace} 	-> 
			Format = "~s: ~p ~p~nContext: ~p~nTrace: ~p~n",
			io:format(standard_error, Format, [Message, Exception, self(), ExcReason, Trace]),
			init:stop(); 
		Else						->
			io:format(standard_error, "~s: ~p ~p~n", [Message, Else, self()])
	end.

strip_escapes(Subject) ->
	{ok, MP} = re:compile("\e\[[\d,\s]+[A-Z]"),
	re:replace(Subject, MP, "", [global, {return, list}]).

%%========================================
%% Functions for keyboard process.
%%========================================

%%@private Export to allow for spawn.
key_start(Pid) -> 
	Pid ! {self(), stderr, io_lib:format("Listening to keyboard ~p~n", [self()])},
	key_loop(Pid, Pid, Pid). 

key_loop(Stdin, Stdout, Stderr) ->
	case io:get_line("") of 
		ok			    ->  receive
								{'EXIT', Stdin, Reason} -> io:format("~s exit: ~s~n", [?MODULE, Reason])
							after 
								1 -> false		
							end;
		eof 			->	key_stop(eof);
		".\n"			->  key_stop(eof);
		{error, Reason} ->  Stderr ! {self(), stderr, io_lib:format("error: ~p~n", [Reason])};
		Line			->  Stdout ! {self(), stdout, Line}
	end,
	key_loop(Stdin, Stdout, Stderr).

key_stop(Reason) ->
    io:format("Stopping: ~p ~p~n", [Reason, self()]),
    exit(Reason).