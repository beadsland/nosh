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

-module(bootstrap).

%%
%% Include files
%%

-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

-export([main/2]).

%%
%% API Functions
%%

main(Param, AppDir) ->
    process_flag(trap_exit, true),

    if Param == [] -> io:format("No command\n"), halt(1); true -> false end,

    case init:get_argument(deps) of
        {ok, [[Deps]]}  -> DepsDir = filename:join(AppDir, Deps);
        _Else           -> DepsDir = filename:join(AppDir, "deps")
    end,
    file:set_cwd(AppDir),
    compile_pose(DepsDir),
    pose:start(Param).

%%
%% Local Functions
%%

% hotswap pose application modules to memory
compile_pose(DepsDir) ->
    PoseEbinDir = filename:join(DepsDir, "pose/ebin"),
    PoseSrcDir = filename:join(DepsDir, "pose/src"),
    
    code:add_pathz(PoseEbinDir),

    WildCard = filelib:wildcard("*.erl", PoseSrcDir),
    {ok, MP} = re:compile("\\.erl\$"),
    Opts = [{return, list}],
    Modules = [re:replace(X, MP, "", Opts) || X <- WildCard],
    compile_app(DepsDir, PoseEbinDir, PoseSrcDir, Modules).

% Compile modules of an application.
compile_app(_DepsDir, _PoseEbinDir, _PoseSrcDir, []) -> true;
compile_app(DepsDir, PoseEbinDir, PoseSrcDir, [Head | _Tail]=Modules) ->
  PoseSrcFile = lists:append(Head, ".erl"),
  Filename = filename:join(PoseSrcDir, PoseSrcFile),
  Options = [verbose, warnings_as_errors, return_errors, binary,
             {outdir, PoseEbinDir}, {i, DepsDir}],
  compile_app(DepsDir, PoseEbinDir, PoseSrcDir, Modules, Filename, Options).

% Compile a module.
compile_app(DepsDir, PoseEbinDir, PoseSrcDir, [Head | Tail], Filename, 
            Options) ->
  case compile:file(Filename, Options) of
    {ok, Module, Binary}        ->
       code:load_binary(Module, Filename, Binary),
       compile_app(DepsDir, PoseEbinDir, PoseSrcDir, Tail);
     error                       ->
       io:format("** ~s: compile failed~n", [Head]), halt();
     {error, Errors, Warnings}   ->
       [First | _Rest] = lists:append(Errors, Warnings),
       {ErrFilename, [ErrorInfo | _MoreErrorInfo]} = First,
       {Line, Module, ErrorDescriptor} = ErrorInfo,
       Where = io_lib:format("~s, line ~p", 
                             [filename:basename(ErrFilename), Line]),
       What = Module:format_error(ErrorDescriptor),
       io:format("** ~s: compile: ~s: ~s~n", [Head, Where, What]),
       halt()
    end.