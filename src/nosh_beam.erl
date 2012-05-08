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

%% @doc Beam binary utility functions used by {@link nosh_load}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-module(nosh_beam).

%%
%% Include files
%%

-include("macro.hrl").

%%
%% Exported Functions
%%
-export([get_binary_detail/2, slurp_binary/1]).

%%
%% API Functions
%%

%% @doc Get version and package of binary
-type attribute() :: atom().
-type beam_lib_error() :: {beam_lib, term()}.
-type binary_detail_error() :: beam_lib_error() | {missing_chunk, attribute()}.
-type version() :: term().
-type package() :: term().
-spec get_binary_detail(Module :: module(), Binary ::  binary()) -> 
          {ok, version(), package()} | {error, binary_detail_error()}.
%
get_binary_detail(Module, Binary) ->
    case beam_lib:version(Binary) of
        {error, beam_lib, What} -> {error, {beam_lib, What}};
        {ok, {Module, Version}} -> get_binary_detail(Module, Binary, Version)
    end.

%% @doc Read binary file into memory.
-type posix() :: atom().
-type file_error_reason() :: posix() | badarg | terminated | system_limit.
-type slurp_error() :: {read, file_error_reason()} | beam_lib_error() 
                        | no_module.
-type filename() :: file:filename().
-spec slurp_binary(Filename :: filename()) -> {ok, module(), binary()}
                                                | {error, slurp_error()}.
%
slurp_binary(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary}    -> slurp_binary(Filename, Binary);
        {error, What}   -> {error, {read, What}}
    end.

%%
%% Local Functions
%%

%%%
% Get binary detail
%%%

% Get package attribute of binary
get_binary_detail(Module, Binary, Version) ->       
    case read_beam_attribute(Binary, package) of
        {error, What}   -> {error, {read_beam, What}};
        {ok, Package}   -> {ok, Version, Package};
        noattr          -> get_binary_detail(Module, Binary, Version, noattr)
    end.

% Figure out explicit package if no attribute found
get_binary_detail(Module, _Binary, Version, noattr) ->
    ModStr = atom_to_list(Module),
    case string:rstr(ModStr, ".") of
        0       -> {ok, Version, ''};
        Last    -> PackStr = string:substr(ModStr, 0, Last-1),
                   Package = list_to_atom(PackStr),
                   {ok, Version, Package}
    end.

%%%
% Slurp binary
%%%

% Extract meta information about binary.
slurp_binary(NewFile, Binary) ->
    case beam_lib:info(Binary) of
        {error, beam_lib, Reason}   -> 
            {error, {beam_lib, Reason}};
        InfoList                    -> 
            slurp_binary(NewFile, Binary, InfoList)
    end.

% Return binary with its fully qualified module name.
slurp_binary(_NewFile, Binary, Info) ->
    case lists:keyfind(module, 1, Info) of
        {module, Module}    -> {ok, Module, Binary};
        false               -> {error, nomodule}
    end.

%%%
% Read beam attribute.
%%% 

%% @doc Retrieve attributes chunk.
read_beam_attribute(Binary, Attribute) ->
    case beam_lib:chunks(Binary, [attributes], [allow_missing_chunks]) of
        {error, beam_lib, Reason}                   ->
            {error, {beam_lib, Reason}};
        {ok, {_Module, [{attributes, AttrList}]}}   ->
            read_beam_attribute(Binary, Attribute, AttrList)
    end.

% Extract desired attribute value.
read_beam_attribute(_Binary, Attribute, AttrList) ->
    case lists:keyfind(Attribute, 1, AttrList) of
        {Attribute, missing_chunk}  -> {error, {missing_chunk, Attribute}};
        {Attribute, [Value]}        -> {ok, Value};
        {Attribute, Value}          -> ?DEBUG("misformed attribute: ~p~n",
                                              [{Attribute, Value}]),
                                       {ok, Value};
        false                       -> noattr
    end.

