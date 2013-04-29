#!wescript
%% -*- erlang -*-
%%! -kernel error_logger silent

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
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

main(Param) ->
    BinDir = filename:dirname(escript:script_name()),
    AppDir = filename:join(BinDir, ".."),
    EbinDir = filename:join(AppDir, "ebin"),
    SrcDir = filename:join(AppDir, "src"),
    DepsDir = filename:join(AppDir, get_deps_dir()),

    Filename = filename:join(SrcDir, "bootstrap.erl"),
    Options = [verbose, warnings_as_errors, return_errors, binary,
               {outdir, EbinDir}, {i, DepsDir}],

    do_main(Filename, Options, Param, AppDir),    
    wait_for_flush().

% Compile and run bootstrap module.
do_main(Filename, Options, Param, AppDir) ->
    case compile:file(Filename, Options) of
        {ok, Module, Binary}        ->
            code:load_binary(Module, Filename, Binary),
            bootstrap:main(Param, AppDir);
        {error, Errors, Warnings}   ->
            io:format("** bootstrap: ~p~n", [{Errors, Warnings}]);
        _Else                       ->
            io:format("** bootstrap: compile failed\n")
    end.

% Determine dependency directory, in order to include libraries.
get_deps_dir() ->
    case init:get_argument(deps) of 
        error        -> Deps = "deps";
        {ok, Result} -> [[Deps]] = Result
    end,
    Deps.

% Hang around waiting for output buffer to flush.
% (Escript doesn't do this reliably.)
wait_for_flush() ->
    timer:sleep(500),
    init:stop(),
    receive after infinity -> ok end.  