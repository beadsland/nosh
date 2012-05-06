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

-import(proplists).
-define(ATTRIB(Module, Attribute), 
		proplists:get_value(Attribute, Module:module_info(attributes))).
-define(VERSION(Module), ?ATTRIB(Module, version)).

-record(std, {in :: pid(), out :: pid(), err :: pid()}).
-define(IO(In, Out, Err), #std{in=In, out=Out, err=Err}).
-define(IO(Pid), #std{in=Pid, out=Pid, err=Pid}).

-import(io_lib).
-import(lists).
-define(STDERR(Format, List), 
		IO#std.err ! {stderr, self(), lists:flatten(io_lib:format(Format, List))}).
-define(STDERR(String), ?STDERR(String, [])).
-define(STDOUT(Format, List), 
		IO#std.out ! {stdout, self(), lists:flatten(io_lib:format(Format, List))}).
-define(STDOUT(String), ?STDOUT(String, [])).

% Debug is special case of Stderr
-define(INIT_DEBUG, put(debug, IO#std.err)).
-ifdef(debug).
-define(DEBUG(Format, List), 
		get(debug) ! 
		   {debug, self(), lists:flatten(io_lib:format(Format, List))}).
-else.
-define(DEBUG(F, L), put(debug_garbage, {F,L})).
-endif.
-define(DEBUG(String), ?DEBUG(String, [])).  

