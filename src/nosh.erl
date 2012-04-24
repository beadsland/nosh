%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and 
%% Distribution License, Version 1.0 (the "License"); you may not use 
%% this file except in compliance with the License. You should have 
%% received a copy of the Common Development and Distribution License 
%% along with this software. If not, it can be retrieved online at 
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyrighted [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo. All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc The `nosh' process module, which handles messaging with a paired `noterm' process.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: nosh as application
%% TODO: nosh_command behaviour
%% TODO: nosh as nosh_command
%% TODO: Buffered I/O (pending get_char working in noterm)
%% TODO: File System:  pwd / cd
%% TODO: File System:  *NIX / cygwin / Win32
%% TODO: File System:  NoSQL
%% TODO: Env
%% TODO: Sh commands
%% TODO: Erl commands
%% TODO: Couch commands
%% TODO: Add support for line continuation (currently throws error)

%% @version 0.1.0
-module(nosh).
-export([start/1]).
version() -> Version = "0.1.0", Version.


%% @doc Start nosh, passing Pid of process providing standard i/o messaging.
start(Pid) ->
	start(Pid, Pid, Pid).
	
start(Stdin, Stdout, Stderr) ->
	process_flag(trap_exit, true),
	Stdout ! {self(), stdout, io_lib:format("Starting Nosh ~s nosql shell ~p~n", [version(), self()])},

	Dependency = command,
	case code:load_file(Dependency) of 
		{error, Reason} 	-> io:format(standard_error, "~s: ~p~n", [Dependency, Reason]), 
							   init:stop(); 
		{module, _Module} 	-> CmdVersion = Dependency:version(),
							   Stdout ! {self(), stdout, io_lib:format("Using rev. ~s command line parser~n", [CmdVersion])},
							   loop(Stdin, Stdout, Stderr)
	end.	
		
loop(Stdin, Stdout, Stderr) ->
	Stdout ! {self(), stdout, prompt()},
	receive
		{Stdin, stdout, Line}		-> 	Eval = command:eval(Line, Stdin, Stdout, Stderr),
									 	Stdout ! {self(), stdout, io_lib:format("parse: ~p~n", [Eval])};
		{'EXIT', Stdin, Reason}		-> 	io:format("Stopping on terminal exit: ~p ~p~n", [Reason, self()]), 
										init:stop();
		{'EXIT', ExitPid, Reason}	-> 	io:format(standard_error, "** Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
										init:stop()
	end,
	loop(Stdin, Stdout, Stderr).


prompt() ->
	"> ".