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

%% @version 0.1.2
-module(nosh_load).
-version("0.1.2").

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").
-define(debug, true).
-include("macro.hrl").

%%
%% Exported functions
%%
-export([test/1]).
-export([load/3]).

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).
-define(MODIFIED(Path, Command, Extn), 
		last_modified(?FILENAME(Path, Command, Extn))).

%%
%% API functions
%%

test(IO) ->
	?INIT_DEBUG,
	?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]), 

	?DEBUG("~n"),
	AltPath = "d:/workspace/nosh/ebin/alt",
	load(IO, test, AltPath),
	test:start(),

	?DEBUG("~n"),
	Alt2Path = "d:/workspace/nosh/ebin/alt2",
	load(IO, test, Alt2Path),
	test:start(),

	?DEBUG("~n"),	
	FlatPath = "d:/workspace/nosh/ebin",
	load(IO, test, FlatPath),
	nosh.test:start(),
	test:start(),

	?DEBUG("~ntest: done~n").
	
load(IO, Command, Path) when is_atom(Command) -> 
	load(IO, atom_to_list(Command), Path);
load(IO, Command, Path) -> 
	case ensure_compiled(IO, Command, Path) of
		{error, Errors, Warnings}	-> throw({load_failed, 
											  {compiler, 
											   {Errors, Warnings}}}); 
		{info, no_src}				-> ?DEBUG("l: no source file~n");
		{info, readonly}			-> ?DEBUG("l: readonly binary~n");
		{ok, _Module, _Binary}		-> ?DEBUG("l: compiled~n");
		ok							-> ?DEBUG("l: file current~n") 
    end,
	case ensure_packaged(IO, Command, Path) of
		{error, Errors2, Warnings2}	-> 
			throw({recompile_failed, {Errors2, Warnings2}});
		{info, no_src}				-> 
			throw({recompile_failed, src_file_missing});
		{info, readonly}			-> 
			throw({recompile_failed, beam_file_readonly});
		{ok, Mod, Bin, Vsn, Pkg} 	-> 
			NewFile = ?FILENAME(Path, Command, ".beam"),
			Package = read_beam_attribute(Bin, package),
			?DEBUG("attribute: -package(~p)~n", [Package]),						   
			ensure_loaded(IO, NewFile, Mod, Bin, Vsn, Pkg)
	end. 

%%
%% Local functions
%%

ensure_loaded(IO, NewFile, NewModule, Binary, NewVsn, Package) ->
	case confirm_loaded(NewModule) of
		{ok, OldFile, OldVsn} 	-> 
			test_namespace(IO, OldFile, NewFile, NewModule),

			ensure_loaded(IO, NewFile, NewModule, Binary, NewVsn, Package, 
						  OldFile, OldVsn);
		false					->
			case code:load_binary(NewModule, NewFile, Binary) of
				{module, NewModule}	-> {module, NewModule};
				{error, What}		-> throw({load_failed, What})
			end
	end.

ensure_loaded(_IO, _NewFile, NewModule, _Binary, NewVsn, _Package, 
			  _OldFile, OldVsn) when OldVsn == NewVsn ->
	?DEBUG("~s: already current~n", [NewModule]);
	
ensure_loaded(IO, NewFile, NewModule, Binary, _NewVsn, Package, 
			  OldFile, _OldVsn) ->
	test_flatpackage(IO, OldFile, NewFile, NewModule, Package),
	
	case code:soft_purge(NewModule) of
		false 	-> purge_alert(NewModule, processes()),
				   code:purge(NewModule);
		true 	-> false
	end,
	code:delete(NewModule),
	
	case code:load_binary(NewModule, NewFile, Binary) of
		{module, NewModule}	-> {module, NewModule};
		{error, What}		-> throw({load_failed, What})
	end.

test_namespace(IO, OldFile, NewFile, NewModule) ->
	if OldFile /= NewFile 	->
		   ?STDERR("~s: hotswap namespace collision~n", [NewModule]);
	   true					-> 
			false
	end.

test_flatpackage(IO, OldFile, NewFile, NewModule, Package) ->
  	if OldFile == NewFile, Package == '' -> 
		?STDERR("~s: flat package module unsafe~n", [NewModule]);
	true -> 
		false
	end.

purge_alert(_Module, []) -> ok;
purge_alert(Module, [Head | Tail]) ->
	Head ! {self(), purging, Module},
	purge_alert(Module, Tail).

ensure_packaged(IO, Command, Path) ->
	Filename = ?FILENAME(Path, Command, ".beam"),
	{ok, Module, Binary, Vsn, Package} = slurp_binary(Filename),
	case Package of
		default		-> ensure_packaged(IO, Command, Path, Vsn, Package);
		''			-> ?DEBUG("l: flat package detected~n"),
					   {ok, Module, Binary, Vsn, Package};
		_Else		-> {ok, Module, Binary, Vsn, Package}
	end.

ensure_packaged(IO, Command, Path, Vsn, Package) ->
	?DEBUG("l: default package detected~n"),
	case ensure_compiled(IO, Command, Path, true) of
		{ok, NewModule, NewBinary} 	-> {ok, NewModule, NewBinary, 
										Vsn, Package};
		Other						-> Other
	end.

confirm_loaded(NewModule) ->
	case code:is_loaded(NewModule) of
		{file, Loaded} 	-> Version = ?ATTRIB(NewModule, vsn),
						   ?DEBUG("old: ~p~n", 
								  [{NewModule, Loaded, Version}]),
						   {ok, Loaded, Version};
		false			-> false
	end.

read_beam_attribute(Binary, Attribute) ->
	case beam_lib:chunks(Binary, [attributes], [allow_missing_chunks]) of
		{ok, {_Module, [{attributes, AttrList}]}} ->
			case lists:keyfind(Attribute, 1, AttrList) of
				{Attribute, missing_chunk}	-> 
					{error, missing_chunk};
				{Attribute, [Value]} 		-> 
					Value;
				{Attribute, Value}			-> 
					?DEBUG("misformed attribute: ~p~n", 
						   [{Attribute, Value}]), 
					Value;
				false						-> 
					false
			end
	end.

slurp_binary(NewFile) ->
	?DEBUG("slurping ~s~n", [NewFile]),
	case file:read_file(NewFile) of
		{ok, Binary} 	-> slurp_binary(NewFile, Binary);
		{error, Reason} -> throw({file, {NewFile, Reason}})
	end.

slurp_binary(NewFile, Binary) ->
	?DEBUG("got binary~n"),
	Info = beam_lib:info(Binary),
	{module, Module} = lists:keyfind(module, 1, Info),
	case read_beam_attribute(Binary, package) of
		false	-> ModStr = atom_to_list(Module),
				   case string:rstr(ModStr, ".") of
					   0	-> Package = '';
					   Last	-> PackStr = string:substr(ModStr, 0, Last-1),
							   Package = list_to_atom(PackStr)
				   end;
		Package 	-> Package
    end,
    ?DEBUG("package: ~p~n", [Package]),
    {ok, {Module, Version}} = beam_lib:version(Binary),
    ?DEBUG("new: ~p~n", [{Module, NewFile, Version}]),
    {ok, Module, Binary, Version, Package}.

ensure_compiled(IO, Command, Path) -> 
	ensure_compiled(IO, Command, Path, false).

ensure_compiled(IO, Command, Path, Force) ->
	Writeable = can_write(IO, Path) andalso 
					can_write(IO, ?FILENAME(Path, Command, ".beam")),
	ensure_compiled(IO, Command, Path, Force, Writeable).

ensure_compiled(_IO, _Command, _Path, _Force, Writeable) 
  when Writeable == false -> {info, readonly};
ensure_compiled(_IO, Command, Path, Force, _Writeable) ->
	?DEBUG("writeable: ~s~n", [?FILENAME(Path, Command, ".beam")]),
	case parallel_src(Path, Command) of
		{ok, SrcPath, Project}	-> 
			SrcMod = ?MODIFIED(SrcPath, Command, ".erl"),
			BinMod = ?MODIFIED(Path, Command, ".beam"),
			?DEBUG("compare: ~p > ~p~n", [SrcMod, BinMod]),
			if SrcMod > BinMod; Force	-> 
				   do_compile(SrcPath, Command, Project, Path);
			   true 					->
				   ok
			end;
		no_src 					-> {info, no_src}
	end.
	
do_compile(SrcPath, Command, Project, Path) when is_atom(Project) ->
	file:make_dir(Path),
	Options = [verbose, warnings_as_errors, return_errors, binary,
			   {d, package, Project}, {outdir, Path}, {i, SrcPath}],
	Filename = ?FILENAME(SrcPath, Command, ".erl"),
	?DEBUG("c: ~p~n", [{Filename, Options}]),
	Compile = compile:file(Filename, Options),
	case Compile of
		error						-> 
			{error, [], []};
		{error, Errors, Warnings}	-> 
			{error, Errors, Warnings};
		{ok, ModuleName, Binary} 	-> 
			do_compile(SrcPath, Command, Project, Path, ModuleName, Binary)
	end.

do_compile(_SrcPath, Command, _Project, Path, ModuleName, Binary) ->
	Outfile = ?FILENAME(Path, Command, ".beam"),
	case file:write_file(Outfile, Binary) of
		{error, Reason}	-> {error, [{binary_write, Reason}], []};
		ok				-> {ok, ModuleName, Binary}
	end.

last_modified(Filename) ->
	case file:read_file_info(Filename) of
    	{ok, FileInfo}	-> {ok, FileInfo#file_info.mtime};
		_Else			-> _Else
	end.

can_write(IO, Filename) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}	-> case FileInfo#file_info.access of 
							   write		-> true;
							   read_write 	-> true;
							   _Else 		-> false
						   end;
		{error, enoent}	-> true;  		% File does not exist, so is 
										% writeable if directory is.
		{error, Reason}	-> ?STDERR("file error: ~p~n", 
								   [{Filename, Reason}]), 
						   false
	end.

can_read(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}		-> case FileInfo#file_info.access of 
								   read 		-> true; 
								   read_write 	-> true; 
								   _Else 		-> false 
							   end;
		{error, Reason}		-> ?DEBUG("read error: ~p~n", 
									  [{Filename, Reason}]), 
							   false
	end.
	
parallel_src(Path, Command) ->
	Split = re:split(Path, "/", [{return, list}]),	
	case ebin_to_src(Split) of 
		{true, SrcPath, Project}	-> parallel_src(Path, Command, SrcPath, 
													Project);
		_Else						-> no_src
	end.

parallel_src(_Path, Command, SrcPath, Project) ->
	case can_read(?FILENAME(SrcPath, Command, ".erl")) of
		true 	-> ?DEBUG("readable: ~s~n", 
						  [?FILENAME(SrcPath, Command, ".erl")]), 
				   {ok, SrcPath, Project}; 
		false 	-> no_src
	end.

ebin_to_src([Head | []]) -> 
	if Head == "ebin" -> {true, "src"}; true -> {false, Head} end;
ebin_to_src([Head | Tail]) ->
	case ebin_to_src(Tail) of
		{true, SrcPath, Project}	-> 
			{true, Head ++ "/" ++ SrcPath, Project};
		{true, SrcPath}				-> 
			{true, Head ++ "/" ++ SrcPath, list_to_atom(Head)};
		{false, BinPath}			-> 
			if Head == "ebin" 	-> {true, "src/" ++ BinPath}; 
			   true 				-> {false, Head ++ "/" ++ BinPath} 
			end
	end.