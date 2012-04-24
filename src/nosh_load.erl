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

%% @doc This is a preliminary draft of the module loader for `nosh'.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: command load module
%% 
%% Pack:  if module actually called as package, look it up on path that way
%%        check if is_loaded path clashes, per flat
%% Deep:  confirm package path is nosh.<i>project</i>.<i>module</i>
%%        reset package path according to shell path
%% Flat:  check if is_loaded path is different from to_be_loaded path
%%        throw error if it is
%%
%% TODO: rebar dependency directory structure
%% TODO: built-in functions project
%% TODO: $PATH search
%% TODO: conservative module loader

%% @version 0.0.0
-module(nosh_load).
-version("0.0.0").

-export([test/0]).

test() -> ok.
	