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

%% @doc The `nosh' process module, which handles messaging with a paired `noterm' process.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.2
-module(nosh).
-version("0.1.2").

%%
%% Include files
%%

-include("macro.hrl").

%%
%% Exported functions
%%

-export([start/1]).

%%
%% API functions
%%

%% @doc Start nosh, passing Pid of process providing standard i/o messaging.
start(Pid) -> start(Pid, Pid, Pid).

%%
%% Local functions
%%

start(Stdin, Stdout, Stderr) ->
	error_logger:tty(false),
	process_flag(trap_exit, true),
	?INIT_DEBUG(Stderr), 
	Stdout ! {self(), stdout, io_lib:format("Starting Nosh ~s nosql shell ~p~n", [?VERSION(?MODULE), self()])},
	?DEBUG("Using rev. ~s command line parser~n", [?VERSION(nosh_parse)]),
	?DEBUG("Using rev. ~s module loader~n", [?VERSION(nosh_load)]),
	
	nosh_load:test(Stderr),

	loop(Stdin, Stdout, Stderr).
		
loop(Stdin, Stdout, Stderr) ->
	Stdout ! {self(), stdout, prompt()},
	receive
		{Stdin, stdout, Line}		-> 	Eval = nosh_parse:parse(Line, Stderr),
										Stdout ! {self(), stdout, io_lib:format("parse: ~p~n", [Eval])};
		{'EXIT', Stdin, Reason}		-> 	?DEBUG("Stopping on terminal exit: ~p ~p~n", [Reason, self()]), 
										init:stop();
		{'EXIT', ExitPid, normal}	-> 	?DEBUG("Saw process exit: ~p~n", [ExitPid]);
		{'EXIT', ExitPid, Reason}	-> 	?STDERR("Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
										init:stop()
	end,
	loop(Stdin, Stdout, Stderr).

prompt() ->	"nosh> ".