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

%% @doc This is a preliminary draft of the command line evaluation module for `nosh'.
%%
%% <b>Draft Notes:</b>
%%
%% In first position, parentheses (`(...)') group commands within for 
%% execution in a subshell environment, as per command substitution rules.  
%% A parentheses group appearing in second position, following a command, is
%% treated as an Erlang function parameter List.  Parentheses groups
%% after second position are invalid, resulting in an evaluation error.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: Tilde expansion
%% TODO: Reserved words
%% TODO: $ - Parameter expansion
%% TODO: ${...} - Parameter expansion
%% TODO: $(...) - Command substitution
%% TODO: $((...)) - Arithmetic Expansion
%% TODO: Fileglobs
%% TODO: Redirection
%% TODO: Here document

%% @version 0.0.0
-module(nosh_eval).
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