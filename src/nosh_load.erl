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
%% TODO: rebar dependency directory structure
%% TODO: built-in functions project
%% TODO: $PATH search
%% TODO: module binary service (to avoid repetitive slurps)
%% TODO: conservative module loader

%% @version 0.0.6
-module(nosh_load).
-version("0.0.6").

-include_lib("kernel/include/file.hrl").
-include("macro.hrl").

-export([test/1]).
-export([load/3]).

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).
-define(MODIFIED(Path, Command, Extn), last_modified(?FILENAME(Path, Command, Extn))).

test(Stderr) ->
	?INIT_DEBUG(Stderr),
	?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]),

	?DEBUG("~n"),
	AltPath = "d:/workspace/nosh/ebin/alt",
	load(test, AltPath, Stderr),
	test:start(),

	?DEBUG("~n"),
	Alt2Path = "d:/workspace/nosh/ebin/alt2",
	load(test, Alt2Path, Stderr),
	test:start(),

	?DEBUG("~n"),	
	FlatPath = "d:/workspace/nosh/ebin",
	load(test, FlatPath, Stderr),
	nosh.test:start(),
	test:start(),

	?DEBUG("~ntest: done~n").
	
load(Command, Path, Stderr) when is_atom(Command) -> load(atom_to_list(Command), Path, Stderr);
load(Command, Path, Stderr) -> 
	case ensure_compiled(Command, Path, Stderr) of
		error						-> throw({load_failed, unspecified_compiler_error});
		{error, Errors, Warnings}	-> throw({load_failed, {compiler, {Errors, Warnings}}}); 
		{info, no_src}				-> ?DEBUG("l: no source file~n");
		{info, readonly}			-> ?DEBUG("l: readonly binary~n");
		{ok, _Module, _Binary}		-> ?DEBUG("l: compiled~n");
		ok							-> ?DEBUG("l: file current~n") 
    end,
	case ensure_packaged(Command, Path, Stderr) of
		error						-> throw({recompile_failed, unspecified_compiler_error});
		{error, Errors2, Warnings2}	-> throw({recompile_failed, {Errors2, Warnings2}});
		{info, no_src}				-> throw({recompile_failed, src_file_missing});
		{info, readonly}			-> throw({recompile_failed, beam_file_readonly});
		{ok, Module, Binary, Vsn} 	-> NewFile = ?FILENAME(Path, Command, ".beam"),
									   ?DEBUG("attribute: -package(~p)~n", [read_beam_attribute(Binary, package)]),
									   ensure_loaded(NewFile, Module, Binary, Vsn, Stderr)
	end.

ensure_loaded(NewFile, NewModule, Binary, NewVsn, Stderr) ->
	case confirm_loaded(NewModule) of
		{ok, OldFile, OldVsn} 	-> 
			if OldFile /= NewFile 	-> ?STDERR("~s: hotswap namespace collision~n", [NewModule]);
			   true					-> false
			end,
			if OldVsn /= NewVsn		-> case code:load_binary(NewModule, NewFile, Binary) of
										   {module, NewModule}	-> {module, NewModule};
										   {error, What}		-> throw({load_failed, What})
									   end;
			   true 				-> false 
			end;
		false					->
			case code:load_binary(NewModule, NewFile, Binary) of
				{module, NewModule}	-> {module, NewModule};
				{error, What}		-> throw({load_failed, What})
			end
	end.

ensure_packaged(Command, Path, Stderr) ->
	Filename = ?FILENAME(Path, Command, ".beam"),
	{ok, Module, Binary, Vsn, Package} = slurp_binary(Filename),
	case Package of
		default		-> ?DEBUG("l: default package detected~n"),
					   case ensure_compiled(Command, Path, Stderr, true) of
						   {ok, NewModule, NewBinary} 	-> {ok, NewModule, NewBinary, Vsn};
						   Other						-> Other
					   end;
		_Else		-> {ok, Module, Binary, Vsn}
	end.

confirm_loaded(NewModule) ->
	case code:is_loaded(NewModule) of
		{file, Loaded} 	-> ?DEBUG("old: ~p~n", [{NewModule, Loaded, ?ATTRIB(NewModule, vsn)}]),
						   {ok, Loaded, ?ATTRIB(NewModule, vsn)};
		false			-> false
	end.

read_beam_attribute(Binary, Attribute) ->
	case beam_lib:chunks(Binary, [attributes], [allow_missing_chunks]) of
		{ok, {_Module, [{attributes, AttrList}]}} ->
			case lists:keyfind(Attribute, 1, AttrList) of
				{Attribute, missing_chunk}	-> {error, missing_chunk};
				{Attribute, [Value]} 		-> Value;
				{Attribute, Value}			-> ?DEBUG("misformed attribute: ~p~n", [{Attribute, Value}]), Value;
				false						-> false
			end
	end.

slurp_binary(NewFile) ->
	?DEBUG("slurping ~s~n", [NewFile]),
	case file:read_file(NewFile) of
		{ok, Binary} 	-> ?DEBUG("got binary~n"),
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
						   {ok, Module, Binary, Version, Package};
		{error, Reason} -> throw({file, {NewFile, Reason}})
	end.

ensure_compiled(Command, Path, Stderr) -> ensure_compiled(Command, Path, Stderr, false).
ensure_compiled(Command, Path, Stderr, Force) ->
	Writeable = can_write(Path, Stderr) andalso can_write(?FILENAME(Path, Command, ".beam"), Stderr),
	if Writeable 	->
		   ?DEBUG("writeable: ~s~n", [?FILENAME(Path, Command, ".beam")]),
		   case parallel_src(Path, Command) of
			   {ok, SrcPath, Project}	-> 
				   SrcMod = ?MODIFIED(SrcPath, Command, ".erl"),
				   BinMod = ?MODIFIED(Path, Command, ".beam"),
				   ?DEBUG("compare: ~p > ~p~n", [SrcMod, BinMod]),
				   if SrcMod > BinMod 		-> do_compile(SrcPath, Command, Project, Path);
					  Force 				-> do_compile(SrcPath, Command, Project, Path);
					  true					-> ok   
				   end;
			   no_src 					-> {info, no_src} 
		   end;
	   true			-> {info, readonly}
	end.												 

do_compile(SrcPath, Command, Project, Path) when is_atom(Project) ->
	file:make_dir(Path),
	Options = [verbose, warnings_as_errors, return_errors, binary,
			   {d, package, Project}, {outdir, Path}, {i, SrcPath}],
	Filename = ?FILENAME(SrcPath, Command, ".erl"),
	?DEBUG("c: ~p~n", [{Filename, Options}]),
	Compile = compile:file(Filename, Options),
	case Compile of
		error								-> error;
		{error, Errors, Warnings}			-> {error, Errors, Warnings};
		{ok, ModuleName, Binary} 			-> Outfile = ?FILENAME(Path, Command, ".beam"),
											   case file:write_file(Outfile, Binary) of
											   		{error, Reason}	-> {error, [{binary_write, Reason}], []};
													ok				-> {ok, ModuleName, Binary}
											   end
	end.

last_modified(Filename) ->
	case file:read_file_info(Filename) of
    	{ok, FileInfo}	-> {ok, FileInfo#file_info.mtime};
		_Else			-> _Else
	end.

can_write(Filename, Stderr) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}	-> case FileInfo#file_info.access of write -> true; read_write -> true; _Else2 -> false end;
		{error, enoent}	-> true;  				% file does not exist, so is writeable if directory is
		{error, Reason}	-> ?STDERR("file error: ~p~n", [{Filename, Reason}]), false
	end.

can_read(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, FileInfo}		-> case FileInfo#file_info.access of read -> true; read_write -> true; _Else2 -> false end;
		{error, Reason}		-> ?DEBUG("read error: ~p~n", [{Filename, Reason}]), false
	end.
	
parallel_src(Path, Command) ->
	Split = re:split(Path, "/", [{return, list}]),	
	case ebin_to_src(Split) of 
		{true, SrcPath, Project} 	-> case can_read(?FILENAME(SrcPath, Command, ".erl")) of
										   true 	-> ?DEBUG("readable: ~s~n", [?FILENAME(SrcPath, Command, ".erl")]),
													   {ok, SrcPath, Project}; 
										   false 	-> no_src 
									   end;
		_Else						-> no_src
	end.

ebin_to_src([Head | []]) -> if Head == "ebin" -> {true, "src"}; true -> {false, Head} end;
ebin_to_src([Head | Tail]) ->
	case ebin_to_src(Tail) of
		{true, SrcPath, Project}	-> {true, Head ++ "/" ++ SrcPath, Project};
		{true, SrcPath}				-> {true, Head ++ "/" ++ SrcPath, list_to_atom(Head)};
		{false, BinPath}			-> if Head == "ebin" 	-> {true, "src/" ++ BinPath}; 
										  true 				-> {false, Head ++ "/" ++ BinPath} end
	end.
		