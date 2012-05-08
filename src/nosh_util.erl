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

%% @doc Utility function library.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo spec API functions

-module(nosh_util).

%%
%% Include files
%%

-include("macro.hrl").

%%
%% Exported Functions
%%
-export([send_stderr/2, send_stdout/2, format_erlerr/1]).

%%
%% API Functions
%%

%% @doc Smart STDOUT/1 macro function.
-type output() :: {atom(), any()} | string().
-spec send_stdout(IO :: #std{}, What :: output()) -> no_return().
send_stdout(IO, What) ->
	Erlout = is_erldata(What), 
	case Erlout of 
		true 	-> IO#std.out ! {erlout, self(), What}; 
		false 	-> ?STDOUT(What, []) 
	end.

%% @doc Smart STDERR/1 macro function.
-spec send_stderr(IO :: #std{}, What :: output()) -> no_return().
send_stderr(IO, What) ->
	Erlerr = is_erldata(What), 
	case Erlerr of 
		true 	-> IO#std.err ! {erlerr, self(), What}; 
		false 	-> ?STDERR(What, []) 
	end.

%% @doc Smartly format erlerr messages.
-spec format_erlerr(What :: any()) -> string().
format_erlerr(What) ->
    case What of 
        {Atom, Data} when is_atom(Atom) ->
            io_lib:format("~p: ~s", [Atom, format_erlerr(Data)]);
        List when is_list(List) ->
            io_lib:format("~s", [List]);
        _Else ->
            io_lib:format("~p", [What])
    end.

%%
%% Local Functions
%%

%% @doc Used by STDOUT and ERROUT macros.
is_erldata(What) ->
	if is_tuple(What); is_atom(What)	-> true;
       is_list(What)					-> false
	end.

