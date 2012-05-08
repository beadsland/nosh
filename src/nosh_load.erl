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
%%
%% <b>Draft Notes:</b>
%%
%% <i>The PATH search routine is not part of this module.</i>
%%
%% Each Erlang module is treated as an executable in `nosh'.  When the 
%% name of a module appears in first position on a `nosh' command line, a 
%% matching `.beam' file is sought on each directory on the `PATH' 
%% environment variable, with one modification:  For each directory on 
%% `PATH' that ends in `ebin\', and for which the current user has write 
%% access, `nosh' will look for a parallel `src\' directory, and if found, 
%% search for a matching `.erl' file therein.
%% 
%% If an associated `.erl' file is found, and it is newer that the `.beam' 
%% file, or if an `.erl' file is found for which no `.beam' file appears, 
%% the `.erl' file will be compiled to its `ebin\' directory.  If this 
%% compilation is successful, the module will be loaded and evaluation 
%% and execution proceeds.  Otherwise, the compiler error is written to 
%% `stdout' and a non-zero status is returned.  
%% 
%% If no associated `.erl' file is found, the `.beam' file on the `PATH'
%% is loaded and evaluation and execution goes forward.  If no `.beam' 
%% file is found, the search continues to the next directory on `PATH', 
%% returning an error if no `.beam' file can be found or compiled from 
%% source before the `PATH' is exhausted.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: document this module
%% TODO: force purge option
%% TODO: module binary service (to avoid repetitive slurps)
%% TODO: conservative module loader

%% @version 0.1.3
-module(nosh_load).
-version("0.1.3").

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").
-define(debug, true).
-include("macro.hrl").

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).


%%
%% Exported functions
%%

-export([run/2, test/1]).

%%
%% API functions
%%

%% Locate command on PATH, load from file if newer than currently loaded.
-type command() :: string() | atom().
-spec run(IO :: #std{}, Command :: command()) -> no_return().
%%
%% @todo get PATH from environment
run(IO, Command) when is_atom(Command) -> run(IO, atom_to_list(Command));
run(IO, Command) ->
	?INIT_DEBUG,
	Path = [filename:absname("ebin")],
	run(IO, Command, Path).

%% Test that we can throw appropriate warnings in various scenarios.
%% @deprecated
test(IO) ->
	?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]), 

	Root = filename:absname(""),
	
	{module, Mod1} = run(IO, "test", [?FILENAME(Root, "ebin/alt")]),
	Mod1:start(),

	{module, Mod2} = run(IO, "test", [?FILENAME(Root, "ebin/alt2")]),
	Mod2:start(),

	{module, Mod3} = run(IO, "test", [?FILENAME(Root, "ebin")]),
	Mod3:start(),
	test:start(),

	?DEBUG("test: done~n").

%%
%% Local functions
%%

% Iterate over path list in search of command.
run(_IO, _Command, []) -> {error, notfound};
run(IO, Command, [Head | Tail]) ->
	case ensure_compiled(Command, Head) of
		{info, nobin}				-> run(IO, Command, Tail);
		{info, Info}				-> ?DEBUG("l: ~p~n", [Info]),
									   run(IO, Command, Head, slurp);
		{ok, _Filename}				-> run(IO, Command, Head, slurp);
		{ok, Module, Binary}		-> run(IO, Command, Head, Module, Binary);
		{error, What}				-> ?STDERR({load, What}),
									   {error, {load, What}}
	end.

% Having found command, slurp binary from file.
run(IO, Command, Dir, slurp) ->
	Filename = ?FILENAME(Dir, Command, ".beam"),
	case slurp_binary(Filename) of
		{ok, Module, Binary}	-> run(IO, Command, Dir, Module, Binary);
		{error, What}			-> {error, {slurp, What}}
	end.

% Load new current module from binary.
run(IO, Command, Dir, OrigModule, Binary) ->
	case run_load(Command, Dir, OrigModule, Binary) of
		{ok, Module, diff_path}	-> ?STDERR({Module, "namespace collision"}),
								   {module, Module};
		{ok, Module, flat_pkg} 	-> ?STDERR({Module, "flat package unsafe"}),
								   {module, Module};
		{ok, Module}			-> ?DEBUG("got module: ~p~n", [Module]),
								   {module, Module};
		{error, What}			-> {error, What}
	end.
  
%%%
% Run load
%%%

% Get version and package details from binary.
run_load(Cmd, Dir, Module, Binary) ->
	case get_binary_detail(Module, Binary) of
		{error, What}			-> 
			{error, {get_detail, What}};
		{ok, Version, Package}	-> 
			run_load(Cmd, Dir, Module, Binary, Version, Package)
	end.

% Make sure binary was compiled using any explicit package attribute.
run_load(Cmd, Dir, Module, Binary, Version, Package) ->
	case ensure_packaged(Cmd, Dir, Package) of
		{error, What}			-> 
			{error, {load, What}};
		{ok, NewMod, NewBin, NewVsn, NewPkg}	->
			run_load(Cmd, Dir, NewMod, NewBin, NewVsn, NewPkg, pack_true);
		ok				-> 
			run_load(Cmd, Dir, Module, Binary, Version, Package, pack_true)
	end.

% Make sure the binary is what is current in memory.
run_load(Cmd, Dir, Module, Binary, Version, Package, pack_true) ->
	Filename = ?FILENAME(Dir, Cmd, ".beam"),
	ensure_loaded(Module, Filename, Binary, Version, Package).		
		
%%%
% Ensure loaded
%%%

% Check if module is currently loaded.
ensure_loaded(Module, BinFile, Bin, Vsn, Pkg) ->
	case code:is_loaded(Module) of
		{file, MemFile}	-> 
			ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, MemFile);
		false			->
			ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false)
	end.

% Figure out if new version of file needs to be loaded.
ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false) ->
	ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false, not_loaded);
ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile) ->
	MemVsn = ?ATTRIB(Module, vsn),
	if BinFile == MemFile, BinVsn == MemVsn, Pkg /= '' 	-> 
		   {ok, Module};
	   BinFile == MemFile, BinVsn == MemVsn				->
		   {ok, Module, flat_pkg};
	   BinFile /= MemFile					  			->
			ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile, diff_path);
	   BinVsn /= MemVsn								->
			ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile, diff_vsn)
	end.

% Load the new module version.
ensure_loaded(Module, BinFile, Bin, _BinVsn, Pkg, _MemFile, Why) ->
	if Why /= not_loaded -> do_purge_delete(Module); true -> false end,
	case code:load_binary(Module, BinFile, Bin) of
		{error, What}		-> 
			{error, {load, What}};
		{module, Module}	->
			case Why of
				not_loaded	-> if Pkg == '' -> {ok, Module, flat_pkg};
								  true		-> {ok, Module} end;
				diff_vsn	-> if Pkg == '' -> {ok, Module, flat_pkg};
								  true		-> {ok, Module} end;
				diff_path	-> {ok, Module, diff_path}
			end
	end.

%%%
% Do purge and delete
%%%

% Attempt a soft purge and then delete.
do_purge_delete(Module) ->
	case code:soft_purge(Module) of
		false 	-> do_purge_delete(Module, processes());
		true 	-> code:delete(Module)
	end.

% Broadcast that a hard purge is about to happen, then purge and delete.
do_purge_delete(Module, []) -> code:purge(Module), code:delete(Module);
do_purge_delete(Module, [Head | Tail]) ->
	Head ! {self(), purging, Module},
	do_purge_delete(Module, Tail).

%%%
% Ensure packaged
%%%

% Force recompilation if explicit package of 'default'
ensure_packaged(Command, Dir, default) ->
	case ensure_compiled(Command, Dir, true) of
		{ok, Module, Binary} 	-> 
			ensure_packaged(Command, Dir, Module, Binary);
		{error, What} 			-> 
			{error, {recompile, What}}
	end;
ensure_packaged(_Command, _Dir, _Package) -> ok.

% Return binary with details
ensure_packaged(_Command, _Dir, Module, Binary) ->
	case get_binary_detail(Module, Binary) of
		{error, What}			-> {error, {get_detail, What}};
		{ok, Version, Package}	-> {ok, Module, Binary, Version, Package}
	end.

%%%
% Ensure compiled
%%%

% By default, we don't force compilation.
ensure_compiled(Command, Path) -> ensure_compiled(Command, Path, false).

% Check if we can write to the ebin directory.
ensure_compiled(Cmd, Dir, Force) ->
	case can_write(Dir) of
		{error, What}	-> {error, {file, What}};
		false			-> {info, readonly_dir};
		true			-> ensure_compiled(Cmd, Dir, Force, write_dir)
	end.

% Check if we can write to the beam file.
ensure_compiled(Cmd, Dir, Force, write_dir) ->
	Filename = ?FILENAME(Dir, Cmd, ".beam"),
	case can_write(Filename) of
		{error, What}	-> 
			{error, {file, What}};
		false			-> 
			HaveBinary = can_read(?FILENAME(Dir, Cmd, ".beam")),
			if HaveBinary 	-> {info, readonly};
			   true			-> {info, nobin}  % i.e., search next dir in path
			end;
		true			-> ensure_compiled(Cmd, Dir, Force, write_both)
	end;

% Find our source file, and if none found, confirm there is even a binary.
ensure_compiled(Cmd, BinDir, Force, write_both) ->
	case parallel_src(BinDir, Cmd) of
		nosrc					->
			HaveBinary = can_read(?FILENAME(BinDir, Cmd, ".beam")),
			if HaveBinary 	-> {info, nosrc};
			   true			-> {info, nobin}  % i.e., search next dir in path
			end;
		{ok, SrcDir, Project}	-> 
			ensure_compiled(Cmd, BinDir, Force, SrcDir, Project)
	end.

% Get modification date of source file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj) ->
	SrcFile = ?FILENAME(SrcDir, Cmd, ".erl"),
	case last_modified(SrcFile) of
		{error, What}	-> 
			{error, {file, What}};
		SrcMod			->
			ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod)
	end.

% Get modification date of binary file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod) ->
	BinFile = ?FILENAME(BinDir, Cmd, ".beam"),
	case last_modified(BinFile) of
		{error, What}	->
			{error, {file, What}};
		BinMod 			->
			ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod, BinMod)
	end.

% Compare modification dates and compile if source is newer.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod, BinMod) ->
	if SrcMod > BinMod; Force	-> do_compile(SrcDir, Cmd, Proj, BinDir);
	   true						-> {ok, ?FILENAME(BinDir, Cmd, ".beam")}
	end.

% Make sure we've got a directory to write to.
do_compile(SrcDir, Cmd, Project, BinDir)  ->
	case file:make_dir(BinDir) of
		ok				-> do_compile(SrcDir, Cmd, Project, BinDir, true); 
		{error, eexist} -> do_compile(SrcDir, Cmd, Project, BinDir, true);
		{error, What}	-> {error, {What, BinDir}}
	end.

% Compile to a binary in memory.
do_compile(SrcDir, Cmd, Project, BinDir, true) ->	
	Options = [verbose, warnings_as_errors, return_errors, binary,
			   {d, package, Project}, {outdir, BinDir}, {i, SrcDir}],
	Filename = ?FILENAME(SrcDir, Cmd, ".erl"),
	case compile:file(Filename, Options) of
		error						-> 
			{error, {compile, unspecified_error}};
		{error, Errors, Warnings}	-> 
			{error, {compile, {Errors, Warnings}}};
		{ok, ModuleName, Binary} 	->
			do_compile(SrcDir, Cmd, Project, BinDir, ModuleName, Binary)
	end.

% Write our binary out to file.
do_compile(_SrcDir, Cmd, _Project, BinDir, ModuleName, Binary) ->
	Outfile = ?FILENAME(BinDir, Cmd, ".beam"),
	case file:write_file(Outfile, Binary) of
		{error, What}	-> {error, {What, Outfile}};
		ok				-> {ok, ModuleName, Binary}
	end.

%%%
% Get binary detail
%%%

% Get version of binary
get_binary_detail(Module, Binary) ->
	case beam_lib:version(Binary) of
		{error, beam_lib, What} -> {error, {beam_lib, What}};
		{ok, {Module, Version}}	-> get_binary_detail(Module, Binary, Version)
	end.

% Get package attribute of binary
get_binary_detail(Module, Binary, Version) ->		
	case read_beam_attribute(Binary, package) of
		{error, What}	-> {error, {read_beam, What}};
		{ok, Package}	-> {ok, Version, Package};
		noattr			-> get_binary_detail(Module, Binary, Version, noattr)
	end.

% Figure out explicit package if no attribute found
get_binary_detail(Module, _Binary, Version, noattr) ->
	ModStr = atom_to_list(Module),
	case string:rstr(ModStr, ".") of
		0		-> {ok, Version, ''};
		Last	-> PackStr = string:substr(ModStr, 0, Last-1),
				   Package = list_to_atom(PackStr),
				   {ok, Version, Package}
	end.

%%%
% Slurp binary
%%%

% Read binary file into memory.
slurp_binary(NewFile) ->
	case file:read_file(NewFile) of
		{ok, Binary} 	-> slurp_binary(NewFile, Binary);
		{error, What} 	-> {error, {read, What}}
	end.

% Extract meta information about binary.
slurp_binary(NewFile, Binary) ->
	case beam_lib:info(Binary) of
		{error, beam_lib, Reason}	-> 
			{error, {beam_lib, Reason}};
		InfoList					-> 
			slurp_binary(NewFile, Binary, InfoList)
	end.

% Return binary with its fully qualified module name.
slurp_binary(_NewFile, Binary, Info) ->
	case lists:keyfind(module, 1, Info) of
		{module, Module}	-> {ok, Module, Binary};
		false				-> {error, nomodule}
	end.

%%%
% Read beam attribute.
%%% 

% Retrieve attributes chunk.
read_beam_attribute(Binary, Attribute) ->
	case beam_lib:chunks(Binary, [attributes], [allow_missing_chunks]) of
		{error, beam_lib, Reason} 					->
			{error, {beam_lib, Reason}};
		{ok, {_Module, [{attributes, AttrList}]}} 	->
			read_beam_attribute(Binary, Attribute, AttrList)
	end.

% Extract desired attribute value.
read_beam_attribute(_Binary, Attribute, AttrList) ->
	case lists:keyfind(Attribute, 1, AttrList) of
		{Attribute, missing_chunk}	-> {error, {missing_chunk, Attribute}};
		{Attribute, [Value]} 		-> {ok, Value};
		{Attribute, Value}			-> ?DEBUG("misformed attribute: ~p~n",
											  [{Attribute, Value}]),
									   {ok, Value};
		false						-> noattr
	end.


%%%
% File property functions
%%%

can_write(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}	-> 
			case FileInfo#file_info.access of
				write		-> true;
				read_write 	-> true;
				_Else 		-> false
			end;
		{error, enoent}	-> 
			true;  	% File does not exist, so is writeable if directory is.
		{error, What}	-> 
			{error, {What, Filename}}
	end.

can_read(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}		-> 
			case FileInfo#file_info.access of 
				read 		-> true; 
				read_write 	-> true; 
				_Else 		-> false 
			end;
		{error, What}		-> 
			{error, {What, Filename}}
	end.

last_modified(Filename) ->
	case file:read_file_info(Filename) of
    	{ok, FileInfo}	-> {ok, FileInfo#file_info.mtime};
		{error, enoent}	-> {ok, nofile};
		{error, What}	-> {error, {What, Filename}}
	end.

parallel_src(BinDir, Cmd) ->
	Split = re:split(BinDir, "/", [{return, list}]),	
	case ebin_to_src(Split) of 
		{true, SrcPath, Proj}	-> parallel_src(BinDir, Cmd, SrcPath, Proj);
		_Else					-> nosrc
	end.

parallel_src(_BinDir, Command, SrcDir, Project) ->
	Filename = ?FILENAME(SrcDir, Command, ".erl"),
	case can_read(Filename) of
		true 	-> {ok, SrcDir, Project}; 
		false 	-> nosrc
	end.

ebin_to_src([Head | []]) -> 
	if Head == "ebin" -> {true, "src"}; true -> {false, Head} end;
ebin_to_src([Head | Tail]) ->
	case ebin_to_src(Tail) of
		{true, SrcDir, Project}	-> 
			{true, Head ++ "/" ++ SrcDir, Project};
		{true, SrcDir}				-> 
			{true, Head ++ "/" ++ SrcDir, list_to_atom(Head)};
		{false, BinDir}			-> 
			if Head == "ebin" 	-> {true, "src/" ++ BinDir}; 
			   true 			-> {false, Head ++ "/" ++ BinDir} 
			end
	end.