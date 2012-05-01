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

%% @version 0.1.5
-module(nosh).
-version("0.1.5").

%%
%% Include files
%%

-define(debug, true).
-include("macro.hrl").

%%
%% Exported functions
%%

-export([start/1]).

% private exports
-export([loop/3]).

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

%%@private Export to allow for hotswap.
loop(Stdin, Stdout, Stderr) ->
	Stdout ! {self(), stdout, prompt()},
	receive
		{Stdin, stdout, "hot\n"}	->  hotswap_nosh(Stdout, Stderr); 
		{Stdin, stdout, Line}		-> 	Eval = nosh_parse:parse(Line, Stderr),
										Stdout ! {self(), stdout, io_lib:format("parse: ~p~n", [Eval])};
		{'EXIT', Stdin, Reason}		-> 	?DEBUG("Stopping on terminal exit: ~p ~p~n", [Reason, self()]), 
										init:stop();
		{'EXIT', ExitPid, normal}	-> 	?DEBUG("Saw process exit: ~p~n", [ExitPid]);
		{'EXIT', ExitPid, Reason}	-> 	?STDERR("Exit ~p: ~p ~p~n", [ExitPid, Reason, self()]), 
										init:stop()
	end,
	?MODULE:loop(Stdin, Stdout, Stderr).

% Development hotswapping.  This should be refactored as a command.
hotswap_nosh(Stdout, Stderr) when is_pid(Stdout) ->
	Stdout ! {self(), stdout, "Hotswapping nosh modules\n"},
	hotswap(noterm, Stderr),
	hotswap(nosh, Stderr),
	hotswap_nosh(code:all_loaded(), Stderr);

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
		nosh_load:load(atom_to_list(Module), filename:dirname(Filename), Stderr)
	catch
		{Error, Detail}	->	?STDERR("~p: ~p~nDetail: ~p~n", [Module, Error, Detail]) 
    end.

prompt() ->	"nosh> ".