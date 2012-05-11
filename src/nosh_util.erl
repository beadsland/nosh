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

%% @doc Utility function library.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo spec API functions

-module(nosh_util).

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").

-include("macro.hrl").

%%
%% Exported Functions
%%

% File Properties
-export([can_read/1, can_write/1, last_modified/1, find_parallel_folder/3]).

%%
%% API Functions
%%


%%%
% File property functions
%%%

%% @doc Test if file or directory is writeable.
-type filename() :: string().
-type file_error() :: {error, {atom(), filename()}}.
-spec can_write(Filename :: filename()) -> boolean() | file_error().
%
can_write(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  ->
            case FileInfo#file_info.access of
                write       -> true;
                read_write  -> true;
                _Else       -> false
            end;
        {error, enoent} ->
            true;   % File does not exist, so is writeable if directory is.
        {error, What}   ->
            {error, {What, Filename}}
    end.

%% @doc Test if file or directory is readable.
-spec can_read(Filename :: filename()) -> boolean() | file_error().
can_read(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  ->
            case FileInfo#file_info.access of
                read        -> true;
                read_write  -> true;
                _Else       -> false
            end;
        {error, enoent} -> false;
        {error, What}   -> {error, {What, Filename}}
    end.

%% @doc Get last date and time file last modified.
-type date_time() :: calendar:date_time().
-spec last_modified(Filename :: filename()) ->
          {ok, date_time()} | file_error().
%
last_modified(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  -> {ok, FileInfo#file_info.mtime};
        {error, enoent} -> {ok, nofile};
        {error, What}   -> {error, {What, Filename}}
    end.

%% @doc Walk absolute directory path, finding where parallel would occur.
-type folder() :: nonempty_string().
-type path_string() :: nonempty_string().
-type path_list() :: {folders, [folder()]}.
-type path() :: path_string() | path_list().
-type project() :: atom().
-type parallel_result() :: {false, path_string()} | {true, path_string()}
                           | {true, path_string(), project()}.
-spec find_parallel_folder(OldFlder :: folder(), NewFolder :: folder(),
                           OldPath :: path()) -> parallel_result().
%
find_parallel_folder(OldFldr, NewFldr, OldDir) when is_list(OldDir) ->
  Split = re:split(OldDir, "/", [{return, list}]),
  find_parallel_folder(OldFldr, NewFldr, {folders, Split});
find_parallel_folder(OldFldr, NewFldr, {folders, [Head | []]}) ->
  if Head == OldFldr -> {true, NewFldr}; true -> {false, Head} end;
find_parallel_folder(OldFldr, NewFldr, {folders, [Head | Tail]}) ->
  case find_parallel_folder(OldFldr, NewFldr, Tail) of
    {true, NewDir, Project}                 ->
      {true, lists:append([Head, "/", NewDir]), Project};
    {true, NewDir}                          ->
      {true, lists:append([Head, "/", NewDir]), list_to_atom(Head)};
    {false, OldDir} when Head == OldFldr    ->
      {true, lists:append([NewFldr, "/", OldDir])};
    {false, OldDir}                         ->
      {false, lists:append([Head, "/", OldDir])}
  end.

%%
%% Local Functions
%%


