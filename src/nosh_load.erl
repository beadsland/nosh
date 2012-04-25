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

%% @version 0.0.4
-module(nosh_load).
-version("0.0.4").

-include("macro.hrl").

-export([test/1]).
-export([load/3]).

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).
-define(MODIFIED(Path, Command, Extn), last_modified(?FILENAME(Path, Command, Extn))).

test(Stderr) ->
	?INIT_DEBUG(Stderr),
	?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]),

	AltPath = "d:/workspace/nosh/ebin/alt",
	file:make_dir(AltPath),
	load(test, AltPath, Stderr),
	test:start(),

	Alt2Path = "d:/workspace/nosh/ebin/alt2",
	file:make_dir(AltPath),
	load(test, Alt2Path, Stderr),
	test:start(),
	
	FlatPath = "d:/workspace/nosh/ebin",
	load(test, FlatPath, Stderr),
	test:start(),
	nosh.test:start(),
	?DEBUG("test: done~n").
	
load(Command, Path, Stderr) when is_atom(Command) -> load(atom_to_list(Command), Path, Stderr);
load(Command, Path, Stderr) -> 
	ensure_compiled(Command, Path),
	NewFile = ?FILENAME(Command, Path, ".beam"),
	{ok, Binary, NewModule, NewVsn} = slurp_binary(NewFile),
	case confirm_loaded(NewModule) of
		{ok, OldFile, OldVsn} 	-> 
			if OldFile /= NewFile 	-> ?STDERR("~s: hotswap namespace collision~n", [NewModule]);
			   true					-> false
			end,
			if OldVsn /= NewVsn 	-> Load = code:load_binary(NewModule, NewFile, Binary),
									   ?DEBUG("l: ~p~n", [Load]);
			   true 				-> false 
			end;
		false					->
		Load = code:load_binary(NewModule, NewFile, Binary),
			?DEBUG("l: ~p~n", [Load])
	end,
	?DEBUG("load: finished~n").

confirm_loaded(NewModule) ->
	case code:is_loaded(NewModule) of
		{file, Loaded} 	-> ?DEBUG("old: ~p~n", [{NewModule, Loaded, ?ATTRIB(NewModule, vsn)}]),
						   {ok, Loaded, ?ATTRIB(NewModule, vsn)};
		false			-> false
	end.

slurp_binary(NewFile) ->
	{ok, Binary} = file:read_file(NewFile),
    Info = beam_lib:info(Binary),
	{module, Module} = lists:keyfind(module, 1, Info),
	{ok, {Module, Version}} = beam_lib:version(Binary),
	?DEBUG("new: ~p~n", [{Module, NewFile, Version}]),
	{ok, Binary, Module, Version}.			

ensure_compiled(Command, Path) ->
	Writeable = can_write(Path) andalso can_write(?FILENAME(Path, Command, ".beam")),
	if Writeable 	->
		   case parallel_src(Path, Command) of
			   {ok, SrcPath, Project}	-> 
				   SrcMod = ?MODIFIED(SrcPath, Command, ".erl"),
				   BinMod = ?MODIFIED(Path, Command, ".beam"),
				   if SrcMod > BinMod 		-> Compile = do_compile(SrcPath, Command, Project, Path),
											   ?DEBUG("c: ~p~n", [Compile]); 
					  true 					-> false 
				   end;
			   no_src 					-> false
		   end;
	   true			-> false
	end.												 

do_compile(SrcPath, Command, Project, Path) ->
	Options = [verbose, report, {d, package, Project}, {outdir, Path}, {i, SrcPath}],
	compile:file(?FILENAME(SrcPath, Command), Options).

last_modified(Filename) ->
	case file:file_info(Filename) of
    	{ok, {_, _, _, _, SrcTime, _, _}}	-> {ok, SrcTime};
		_Else								-> _Else
	end.

can_write(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, {_, _, Access, _, _, _, _}}	-> case Access of write -> true; read_write -> true; _Else2 -> false end;
		{error, enoent}						-> true;  % file does not exist, so is writeable if directory is
		Else								-> Else
	end.

can_read(Filename) ->
	case file:read_file_info(Filename) of 
		{ok, {_, _, Access, _, _, _, _}}	-> case Access of read -> true; read_write -> true; _Else2 -> false end;
		Else								-> Else
	end.
	
parallel_src(Path, Command) ->
	Split = re:split(Path, "/", [{return, list}]),	
	case ebin_to_src(Split) of 
		{true, SrcPath, Project} 	-> case can_read(?FILENAME(Path, Command, ".erl")) of
										   true 	-> {ok, SrcPath, Project}; 
										   false 	-> no_src 
									   end;
		_Else						-> no_src
	end.

ebin_to_src([Head | []]) -> if Head == "ebin" -> {true, "src"}; true -> {false, Head} end;
ebin_to_src([Head | Tail]) ->
	case ebin_to_src(Tail) of
		{true, SrcPath, Project}	-> {true, Head ++ "/" ++ SrcPath, Project};
		{true, SrcPath}				-> {true, Head ++ "/" ++ SrcPath, Head};
		{false, BinPath}			-> if Head == "ebin" 	-> {true, "src/" ++ BinPath}; 
										  true 				-> {false, Head ++ "/" ++ BinPath} end
	end.
	
	
			

	
	