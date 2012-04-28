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

% @doc == Introduction ==
% 
% `nosh' is a {@section Bourne Shell Emulator}, {@section Erl Shell Alternative} 
% and {@section NoSQL File System} implemented in Erlang.  Providing an intuitive
% commandline interface for compiling and loading Erlang modules, and for executing
% Erlang module functions, `nosh' affords both the ease of a UNIX shell and the power
% of an Erlang interpreter.
%
% <em><strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't do much yet.</em>
%
% == Contents ==
%
% <ul>
% <li> {@section Noterm} </li>
% <li> {@section Bourne Shell Emulator} </li>
% <li> {@section Erl Shell Alternative} </li>
% <li> {@section NoSQL File System} </li>
% </ul>
%
% == Noterm ==
%
% A `nosh' process is launched by first starting `noterm', which handles keyboard input, sending
% this to the `nosh' process as messages, and presenting output and errors to the user as they
% are received in message form from the `nosh' process.
%
% <i>Full terminal emulation has yet to be implemented.</i>  The `nosh' process continues to 
% run until it receives an end-of-file message from `noterm'.  This is currently produced by
% typing a period (`.') by itself on a line, followed by a `<newline>'.  (This EOF period should
% not be confused with the {@section Erlang Context} left-wise conjunction; or with the period
% command, one of the built-in {@section Bourne Commands}.)
% 
% == Bourne Shell Emulator ==
%
% `nosh' emulates much of the functionality of the Bourne shell (`sh'), including {@section Quoting} and {@section Grouping}.
%
% Currently all `nosh' does is parse a command line and return a tree representing quoting and grouping
% relationships.  <i>Execution functions are not yet implemented</i>.
%
% === Bourne Context ===
%	
% Two syntax regimes are supported by `nosh', identified as Bourne Context and {@section Erlang Context}.
% Bourne Context is the emulation of `sh' syntax, marked by {@section Grouping} rules and parameters
% written as arrays of space-delimited tokens.
% 
% For a discussion of how Erlang modules and functions are used in the Bourne Context, see the 
% {@section Erl Shell Alternative} section.
% 
% === Quoting ===
% 
% Single quoted (`` '...' ''), double quoted (`"..."'), and back quoted (<code>`'...`'</code>) character
% sequences are supported, as per the Bourne shell, as is use of the backslash (`\') to escape special
% characters.  Additionally, single and double quoted sequences have special meaning
% when passed to Erlang functions.  (Unlike in Bourne shell, quote removal does not occur in `nosh'.)
% 
% Single quotes remove the special meaning of all characters they enclose.  In addition, single quotes
% indicate the `atom()' type when passed to Erlang functions.
% 
% Double quotes remove the special meaning of all characters other than the dollar sign (`$'), the
% back quote (<code>`'</code>), the backslash (`\'), and the newline (per {@section Multiline Parsing}).  
% Additionally, double quotes indicate the `string()' type, <i>i.e.</i> a `list()' of characters.  
% Strings are passed to Erlang functions only after all embedded {@section Parameter Expansion}, 
% {@section Command Substition} and {@section Arithmetic Expansion} has been performed.
%
% Back quotes signal {@section Command Substitution}, the back quoted character sequence being replaced
% by the results of the substituted command(s) execution.  A {@section Lists} back quote (<code>[`'...`']</code>)
% results in an Erlang `list()' of tokens.  Otherwise, the results of a back quoted sequence are inserted
% into the Bourne-standard token array as space-delimited parameters.
% 
% Escaped characters work as expected.
% 
% ==== Multiline Parsing ====
% 
% Presently, multi-line parsing is not supported.  Any line ending in a backslash (an escaped carriage return)
% will result in an error.
%  
% === Grouping ===
% 
% Grouping is parsed for entire line prior to execution, unlike Bourne standard.  
% This enforces transactional integrity:  command execution only occurs if entire command sequence parses correctly.
% 
% In first position, parentheses (`(...)') group commands within for execution in a {@section Subshell Environment},
% as per {@section Command Substitution}.  A parentheses group appearing in second position, following
% a command, is treated as an {@section Parameter List}.  Parentheses groups
% after second position are invalid, throwing a parsing error.  (<i>Second and third position
% processing not yet implemented.</i>)
% 
% The faux within-shell execution syntax of Bourne shell (`{ ...; };')--implemented 
% with the open curly bracket (`{') and closed curly bracker (`}') as reserved words--is not
% supported by `nosh', curly brackets instead marking {@section Tuples}.
% 
% The semicolon (`;') conjunction works in `nosh' as it does in Bourne shell, execution of the command group 
% following the semicolon being deferred until the command group preceding the semicolon has completed.
% 
% Likewise, double ampersand (`&&') and double vertical bar (`||') afford conditional execution.  The command
% group following a double ampersand executing only on a zero status (an Erlang return value of `ok' or `{ok, ...}'),
% and the command group following a double vertical bar executing only on a non-zero status (any other Erlang return value).
% 
% These {@section Bourne Context} conjunctions are parsed right-wise.  That is, everything that follows the conjunction
% is returned as a nested group in the last element of the enclosing group context.  There are two left-wise conjunctions,
% the period (`.'), indicating an {@section Erlang Context} group, and the `<newline>', 
% indicating the enclosing command line group.
% 
% ==== Pipes ====
% 
% As per Bourne shell, pipes are marked by a single vertical bar (`|').  Erlang processes may participate in pipe
% relationships if they implement the {@section Nosh_exec Behaviour}.  Otherwise, processes grouped by the single vertical bar
% will run, but each in an isolated {@section Subshell Environment} without any access to piped standard input and output streams.
% 
% ==== Background Jobs ====
%  
% The single ampersand (`&') marks a background job as per `bash' (Bourne Again Shell) syntax.
% <i>The `jobs' command and `%' syntax for bringing jobs to the foreground have not yet been implemented.</i>
% 
% === Parameter Expansion ===
% 
% <i>Not yet implemented.</i>
% 
% === Command Substitution ===
% 
% <i>Not yet implemented</i>.
% 
% === Arithmetic Expansion ===
% 
% <i>Not yet implemented</i>.
% 
% === Subshell Environment ===
% 
% <i>Command execution, including shell and subshell environments, has not yet been implemented.</i>
% 
% === Bourne Commands ===
% 
% <i>Bourne built-in commands have not yet been implemented.</i>
% 
% == Erl Shell Alternative ==
% 
% `nosh' will provide a UNIX-style command line for Erlang, as a human-thinkable alternative to
% the Erl shell ([http://www.erlang.org/doc/man/erl.html]).  
% 
% The Erlang syntax, while perfectly appropriate for functional programming, can prove frustrating in the 
% procedural context of a shell environment.  Further, exception handling in the Erl shell is less than
% graceful, and compiling and loading modules can be unduly cumbersome and non-intuitive.
% 
% For these reasons, `nosh' operates in {@section Bourne Context} by default, while still supporting
% Erlang {@section Lists} and {@section Tuples}.  Within Bourne Context, {@section Modules} compile
% and load on-the-fly, {@section Parameters} may be passed (and functions called) with minimal typing,
% and {@section Exceptions} thrown by processes are gracefully caught and displayed.
% 
% === Modules ===
% 
% <i>None of this has yet been implemented.</i>
% 
% Each Erlang module is treated as an executable in `nosh'.  When the name of a module appears
% in first position on a `nosh' command line, a matching `.beam' file is sought on each directory
% on the `$PATH' environment variable, with one modification.  For each directory on `$PATH' that
% ends in `ebin\', and for which the current user has write access, the shell will look for a 
% parallel `src\' directory, and if found, search for a matching `.erl' file therein.
% 
% If an associated `.erl' file is found, and it is newer that the `.beam' file, or if an `.erl' file
% is found for which no `.beam' file appears, the `.erl' file will be compiled to its `ebin\' 
% directory.  If this compilation is successful, the module will be loaded and {@section Execution} proceeds.  
% Otherwise, the compiler error is written to `stdout' and a non-zero status is returned.  
% 
% If no associated `.erl' file is found, the `.beam' file on the $PATH is loaded and {@section Execution}
% goes forward.  If no `.beam' file is found, the search continues to the next directory on `$PATH',
% returning an error if no `.beam' file can be found or compiled from source before the `$PATH' is
% exhausted.
% 
% ==== Execution ====
% 
% Module execution may be invoked in one of three modes:  implicit start, explicit function or 
% getoptions functions.
% 
% In <i>implicit start</i> mode, the module name appears in first position (as a command) with or without parameters.  
% If without parameters, `nosh' will attempt to spawn a process running the module's `start/4' function, 
% or else `start/3', as specified by the {@section Nosh_exec Behaviour}, and failing that, 
% will attempt to spawn on `start/0', reporting an error on failure.  
% 
% If with parameters, `nosh' will first call any getoptions functions, caching the results from each.
% If any getoptions functions were found, `nosh' will then attempt to spawn a process running the
% module's {@section Nosh_exec Behaviour} `start/6' function, reporting an error on failure.  Otherwise,
% `nosh' will attempt to spawn on `start/5', reporting an error on failure.
% 
% In <i>explicit function</i> mode, the command appears in Erlang syntax as <i>`Module'</i>`:'<i>`Function'</i>.
% This can stand on it's own, or else be followed either by {@section Parameters} or a {@section Parameter List}.  
% The resulting arity function is called by `nosh', reporting an error on failure.  In {@section Erlang Context},
% all function invocations are in explicit function mode, always with a Parameter List.
% 
% In <i>getoptions functions</i> mode, the module name appears in first position, followed by
% {@section Parameters}, including one or more 0-arity and/or 1-arity function names specified in Perl getoptions 
% ([http://aplawrence.com/Unix/perlgetopts.html]) syntax.  These functions are executed in order, before
% `nosh' switches to implicit start mode, as described above.
% 
% ==== Subshell Environment ====
% 
% <i>Not yet implemented.</i>
% 
% ==== Nosh_exec Behaviour ====
% 
% <i>Not yet implemented.</i>
% 
% Canonical spawn entry function:  `start(stdin, stdout, stderr, env, arg, getoptions)'
% 
% === Parameters ===
% 
% Parameters may be Bourne-style {@section Words}, including character sequences delimited by single or double
% {@section Quoting}.  In addition, Erlang-syntax {@section Lists} and/or {@section Tuples} may be
% specified as parameters.  {@section Parameter List} syntax is a special case only valid in 
% explicit function {@section Execution} mode.
% 
% ==== Words ====
% 
% The simplest `nosh' command line consists of a command followed by zero or more words, this
% sequence being delimited by the characters in the `$IFS' environment variable, or `<space>', `<tab>' and `<newline>'
% if `$IFS' is undefined.  Each single and/or double quoted sequence is treated as a word for this purpose 
% (see {@section Quoting}).  
% 
% Unquoted (<i>i.e.</i> bare) words and single quoted words are passed to Erlang functions as type `atom()', while
% double quoted words are passed as type `string()'.
% 
% ==== Lists ====
% 
% Zero or more Words, Lists and/or Tuples are treated as a single List parameter 
% (not to be confused with a {@section Parameter List}) when enclosed by an open square bracket (`[')
% and close square bracket (`]').  Per Erlang syntax, elements of a List are delimited by commas (`,'),
% whitespace is ignored, and `$IFS' specified delimiters have no special meaning.
% 
% {@section Command Substitution} may be used to generate an Erlang List, by nesting back quotes
% within square brackets (<code>[`'...`']</code>).
% 
% ==== Parameter List ====
% 
% A Parameter List follows Erlang syntax for specifying the parameters (and indirectly, the arity) of a
% function, and consists of zero or more comma-delimited Words, Lists, and Tuples enclosed by an 
% open parentheses (`(') and close parentheses (`)') rather than square brackets.  A Parameter List
% may only appear in second position after a command, and only in explicit function mode (see {@section Execution}).
% 
% In first position, parentheses denote command {@section Grouping}.
% 
% ==== Tuples ====
% 
% One or more Words, Lists and/or Tuples are treated as a single Tuple parameter when
% enclosed by an open curly bracket (`{') and close curly bracket (`}').  Per Erlang syntax, 
% elements of a Tuple are delimited by commas (`,'), whitespace is ignored, and `$IFS' specified 
% delimiters have no special meaning.
% 
% === Exceptions ===
% 
% The `nosh' process and/or {@section Subshell Environment} that calls or spawns any given module function 
% catches all exceptions thrown thereby, including those that result in a non-`normal' status process ``'EXIT''''.  
% By default, any such exception is printed to `stderr', followed by a stack trace, and a non-zero exit
% status is passed to any conditional {@section Grouping}.  
% 
% === Erl Commands ===
% 
% <i>Not yet implemented:</i>  All functions available through the Erl shell are available as `nosh' commands.
% 
% === Erlang Context ===
% 
% <i>Not yet implemented:</i>  The Erlang Context is a special case of {@section Grouping}, denoted by the left-wise
% conjunction, period (`.').  When the `nosh' command parser encounters a period on it's own (not as part of any
% {@section Parameters}), it rolls everything in the current context to the left of the period into a sub-context,
% and then continues parsing the sequence that follows within the current context.
%
% Any semicolon to be included in Erlang Context must be escaped (`\;').  Otherwise, the semicolon will be interpreted
% as a grouping conjunction delimiting the start of a context, rather than a syntantical marker within the current
% Erlang Context.
% 
% The period conjunction should not be confused with either the {@section Noterm} end-of-file character or the period
% command, one of the built-in {@section Bourne Commands}.
% 
% == NoSQL File System ==
% 
% <i>Not yet implemented:</i>  `nosh' will provide an NFS-style interface to CouchDB databases and views.
% 
% @end