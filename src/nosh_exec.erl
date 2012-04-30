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

%% @doc This is a preliminary draft of the command line execution module for `nosh'.
%%
%% <b>Draft Notes:</b>
%%
% Module execution may be invoked in one of three modes:  implicit start, explicit function or 
% getoptions functions.
% 
% In <i>implicit start</i> mode, the module name appears in first position (as a command) with or without parameters.  
% If without parameters, `nosh' will attempt to spawn a process running the module's `start/4' function, 
% or else `start/3', as specified by the Nosh_exec Behaviour, and failing that, 
% will attempt to spawn on `start/0', reporting an error on failure.  
% 
% If with parameters, `nosh' will first call any getoptions functions, caching the results from each.
% If any getoptions functions were found, `nosh' will then attempt to spawn a process running the
% module's Nosh_exec Behaviour `start/6' function, reporting an error on failure.  Otherwise,
% `nosh' will attempt to spawn on `start/5', reporting an error on failure.
% 
% In <i>explicit function</i> mode, the command appears in Erlang syntax as <i>`Module'</i>`:'<i>`Function'</i>.
% This can stand on it's own, or else be followed either by Bourne-style parameters or an Erlang function parameter List.  
% The resulting arity function is called by `nosh', reporting an error on failure.  In Erlang Context,
% all function invocations are in explicit function mode, always with a Parameter List.
% 
% In <i>getoptions functions</i> mode, the module name appears in first position, followed by
% parameters, including one or more 0-arity and/or 1-arity function names specified in Perl getoptions 
% ([http://aplawrence.com/Unix/perlgetopts.html]) syntax.  These functions are executed in order, before
% `nosh' switches to implicit start mode, as described above.
%
% Canonical spawn entry function:  `start(stdin, stdout, stderr, env, arg, getoptions)'
%
% === Parameters ===
% 
% Parameters may be Bourne-style words, including character sequences delimited by single or double
% quotes.  In addition, Erlang-syntax data types may be
% specified as parameters.  Function parameter list syntax is a special case only valid in 
% explicit function execution mode.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: implicit start
%% TODO: explicit function
%% TODO: getoptions functions
%% TODO: set env
%% TODO: pattern matching
%% TODO: Alias substitution
%% TODO: git qualified packages

%% @version 0.0.0
-module(nosh_exec).
-version("0.0.0").

%%
%% Include files
%%

%%
%% Exported functions
%%

%%
%% API functions
%%

%%
%% Local functions
%%