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

% Standard I/O
-export([send_stderr/2, send_stdout/2, format_erlerr/1]).

% File Properties
-export([can_read/1, can_write/1, last_modified/1]).


%%
%% API Functions
%%

%%%
% Standard I/O functions
%%%

%% @doc Smart STDOUT/1 macro function.
-type output() :: {atom(), any()} | string().
-spec send_stdout(IO :: #std{}, What :: output()) -> ok.
send_stdout(IO, What) ->
  Erlout = is_erldata(What),
  case Erlout of
    true 	-> IO#std.out ! {erlout, self(), What};
    false 	-> ?STDOUT(What, [])
  end,
    ok.

%% @doc Smart STDERR/1 macro function.
-spec send_stderr(IO :: #std{}, What :: output()) -> ok.
send_stderr(IO, What) ->
  Erlerr = is_erldata(What),
  case Erlerr of
    true 	-> IO#std.err ! {erlerr, self(), What};
    false 	-> ?STDERR(What, [])
  end,
    ok.

%% @doc Smartly format erlerr messages.
-spec format_erlerr(What :: any()) -> string().
format_erlerr(What) ->
    case What of
        {Atom, Data} when is_atom(Atom) ->
            io_lib:format("~p: ~s", [Atom, format_erlerr(Data)]);
        List when is_list(List) ->
            io_lib:format("~s", [List]);
        _Else                   ->
            io_lib:format("~p", [What])
    end.

%%%
% File property functions
%%%

%% @doc Test if file or directory is writeable.
-type filename() :: string().
-type file_error() :: {error, {atom(), filename()}}.
-spec can_write(Filename :: filename()) -> boolean() | file_error().
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
        {error, What}   ->
            {error, {What, Filename}}
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

%%
%% Local Functions
%%

%% @doc Used by STDOUT and ERROUT macros.
is_erldata(What) ->
  if is_tuple(What); is_atom(What)	-> true;
       is_list(What)					-> false
  end.