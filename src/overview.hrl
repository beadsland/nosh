%% CDDL HEADER START    -*-Erlang-*-
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

%% @doc `nosh' is a Bourne shell emulator, Erl shell alternative, and
%% noSQL file system,  implemented in 
%% <a href="http://www.erlang.org/download_release/14">Erlang/OTP R15B01</a>.  
%% Providing an intuitive
%% commandline interface for compiling and loading Erlang modules, and for
%% executing Erlang module functions, `nosh' affords both the ease of a
%% UNIX shell and the power of an Erlang interpreter.
%%
%% <em><strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't
%% do much yet.</em>
%%
%% <ul>
%% <li> {@section Installation} </li>
%% <li> {@section Bourne Shell Emulator} </li>
%% <li> {@section Erl Shell Alternative} </li>
%% <li> {@section NoSQL File System} </li>
%% </ul>
%%
%% == Installation ==
%%
%% To get started with `nosh', confirm you have an Internet connection,
%% and then make the project and run the `nosh' wrapper script, as follows:
%%
%% <pre>
%% make install
%% bin/nosh
%% </pre>
%%
%% It is recommended that the project `bin/' be prefixed to your `PATH',
%% so that `nosh' can be run from any working directory.  Also included
%% in `bin/' is `nano' script that detects if you're running in Wicked
%% Shell (see below) and fires up an external shell to run either the
%% UNIX standard `pico' editor or GNU `nano' editor where available.
%% (`nano'/`pico' requires a tty to run).
%%
%% Users of the Eclipse IDE may integrate `nosh' shell with their
%% workflow by installing the Wicked Shell plugin
%% [http://www.wickedshell.net/] from the Eclipse Marketplace.  Simply
%% run `bash' or `sh' under Wicked Shell, and then execute `nosh'
%% from there.
%%
%% == Bourne Shell Emulator ==
%%
%% `nosh' emulates much of the functionality of the Bourne shell (`sh').
%%
%% Currently all `nosh' does is {@link nosh_parse. parse} a command line
%% and return a tree representing quoting and grouping relationships.
%% <i>Execution functions are not yet implemented</i>.
%%
%% Two syntax regimes are supported by `nosh', identified as Bourne Context
%% and Erlang Context.  Bourne Context is the emulation of `sh' syntax,
%% marked by grouping rules and command parameters written as arrays of
%% space-delimited tokens.
%%
%% A full suite of core Bourne Shell commands are being implemented as part
%% of the <b><a href="http://github.com/beadsland/nosh_bin">nosh_bin</a></b>
%% project.
%%
%% == Erl Shell Alternative ==
%%
%% `nosh' will provide a UNIX-style command line for Erlang, as a
%% human-thinkable alternative to the Erl shell
%% [http://www.erlang.org/doc/man/erl.html].
%%
%% The Erlang syntax, while perfectly appropriate for functional
%% programming, can prove frustrating in the imperative paradigm of a
%% shell environment, and compiling and loading modules can be unduly
%% cumbersome and non-intuitive.
%%
%% `nosh' operates in Bourne Context by default, while still supporting
%% Erlang data types, and allowing Erlang Context to be embedded in
%% commandlines as needed.  Within Bourne Context, Erlang modules are
%% invoked like UNIX commands, and {@link nosh_load. compile and load
%% on-the-fly}, parameters being passed (and functions called) with
%% minimal typing.
%%
%% === Erl Commands ===
%%
%% <i>Not yet implemented:</i>  All functions available through the Erl
%% shell are available as `nosh' commands.
%%
%% === Erlang Context ===
%%
%% <i>Not yet implemented:</i>  Erlang style syntax can be used on the
%% `nosh' command line as per the Erl shell.  See {@link nosh_parse} for
%% more details.
%%
%% == NoSQL File System ==
%%
%% <i>Not yet implemented:</i>  `nosh' will provide an NFS-style interface
%% to CouchDB databases and views.
%%
%% @end