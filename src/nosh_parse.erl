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

%% @doc This is a preliminary draft of the command line parser for `nosh'.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo
%% @reference See <a href="http://sayle.net/book/basics.htm">Shell Basics</a> for overview of functionality.  (to be implemented)
%% @reference See <a href="http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html">Shell Command Language</a> 
%% for detailed specification. (to be implemented)
%% @end

%% TODO: Tokenizing
%% TODO: [...] Lists
%% TODO: [`'...`'] Lists command substituion
%% TODO: (...) second position parameter Lists
%% TODO: {...} Tuples
%% TODO: `<...>' Pids
%% TODO: `<<...>>' Bitstrings
%% TODO: Erlang Stack
%% TODO: Line continuation

%% @version 0.1.3
-module(nosh_parse).
-version("0.1.3").

%%
%% Include files
%%

-include("macro.hrl").

-define(context_CHARS, "\\\\\"\'\`\n").
-define(GROUP_CHARS, "\;\(\)\&\|").     % curly braces are reserved words, not grouping characters
-define(SPACE_CHARS, "\ \t\n").

%%
%% Exported functions
%%

-export([parse/2]).

%%
%% API functions
%%

%% @doc Parse command line string and return a list of nested quoting and grouping Stack blocks, 
%% or else `failed' on a caught syntax exception.
%%
%% Handle thrown errors for unmatched quoting and grouping characters.
%% @end
-type io_proc() :: pid().
-type quote_type() :: back | doub | sing | escp | dbcp.
-type group_type() :: line | pren | ifok | ambi | ifnz | pipe.
-type context_type() :: {eval, eval} | {context, group_type()} | {context, quote_type()}.
-type block() :: nonempty_string() | {context_type(), list(block())}.
-spec parse(Subject :: nonempty_string(), Stderr :: io_proc()) -> failed | list(block()).
%%
parse(Subject, Stderr) ->
	Pattern = io_lib:format("([~s~s~s])", [?context_CHARS, ?GROUP_CHARS, ?SPACE_CHARS]),
	{ok, MP} = re:compile(Pattern),

	Split = re:split(Subject, MP, [{return, list}]),
	Pred = fun(T) -> case T of [] -> false; _Else -> true end end,
	CleanSplit = lists:filter(Pred, Split), 
	
	QuoteErr = "Quote error: Closing ~s missing~n", 
	GroupErr = "Group error: Closing ~s missing~n", 
	try close_context(line, [eval], CleanSplit) of
		{Parse, [eval]} -> [{{context, line}, StackList}, close_eval] = Parse, StackList	
	catch
		{eval, eval}  		-> ?STDERR("Eval error: shouldn't happen~n"), failed;
		{context, line} 	-> ?STDERR(GroupErr, ["EOL"]), failed;
		{context, semi}		-> ?STDERR(GroupErr, ["EOL"]), failed;
		{context, pren}		-> ?STDERR(GroupErr, ["\)"]), failed;
		{close, pren}		-> ?STDERR("Group error: Unmatched closing parentheses~n"), failed;
		{context, back} 	-> ?STDERR(QuoteErr, ["\`"]), failed;
		{context, doub} 	-> ?STDERR(QuoteErr, ["\""]), failed;
		{context, sing} 	-> ?STDERR(QuoteErr, ["\'"]), failed;
		{context, escp} 	-> ?STDERR("context error: Line continuation not supported~n"), failed
	end. 

%%
%% Local functions
%%

%% Parse list of strings split on quoting and grouping characters, according to current Stack type.
%% Return tuple of block list and Stack stack OR tuple of 'close_context', Stack stack, and trailing Stack tree.
%% Throw exception for unmatched quoting or grouping character.
parse(eval, [], []) -> {[close_eval], []};
parse(Type, _Stack, []) -> throw(Type);
parse(Type, Stack, [[] | Tail]) -> parse(Type, Stack, Tail);
parse(Type, Stack, [Head | Tail]) when is_integer(Head) ->
	HeadStr = io_lib:format([Head], []),  
	parse(Type, Stack, HeadStr ++ Tail);
parse({context, QType}, Stack, List) -> 	
	?DEBUG("parse_context(~p, ~p, ~p)~n", [QType, Stack, List]), 	
	Parse = parse_context(QType, Stack, List), 
	?DEBUG("~nparse_context(~p, ~p, ~p) ->~n     ~p~n", [QType, Stack, List, Parse]), 
										 
	case Parse of 							
		{close_context, Stack, Tail}	-> Close = {close_context, QType},
										   [SuperType | SuperStack] = Stack,
										   {Post, _ReturnStack} = parse(SuperType, SuperStack, Tail),
										   {[Close] ++ Post, SuperStack};
		{Tail, ReturnStack}			-> {Tail, ReturnStack}
	end.  

%% Wind up context block.
close_context(QType, Stack, List) ->
	?DEBUG("# parse_context(~p, ~p, ~p)~n", [QType, Stack, List]),
	{Tail, _ReturnStack} = parse({context, QType}, Stack, List), 
	Close = {close_context, QType},
	Pred = fun(T) -> T /= Close end,
	{L1, L2} = lists:splitwith(Pred, Tail),
	if
		QType == dbcp, L1 == []	-> Context = "\\";      % Didn't escape anything, so restore backslash as regular character.
		true					-> Context = {{context, QType}, L1}
	end,
	{[Context] ++ lists:delete(Close, L2), Stack}. 


%% @doc Unwind context and group stream.
parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\n" -> {close_context, Stack, Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\n" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\n" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\n" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\n" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\n" -> {close_context, Stack, [Char] ++ Tail};

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\;" -> close_context(semi, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\"" -> close_context(doub, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == doub, Char == "\"" -> {close_context, Stack, Tail};

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\\" -> close_context(escp, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == doub, Char == "\\" -> close_context(dbcp, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\'" -> close_context(sing, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == sing, Char == "\'" -> {close_context, Stack, Tail};

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == doub, Char == "\`" -> close_context(back, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\`" -> {close_context, Stack, Tail};
  
parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\(" -> close_context(pren, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "\)" -> {close_context, Stack, Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "\)" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "\)" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "\)" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "\)" -> {close_context, Stack, [Char] ++ Tail};
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "\)" -> {close_context, Stack, [Char] ++ Tail};
parse_context(_QT, _Stack, [Char | _Tail]) when Char == "\)" -> throw({close, pren});
 
parse_context(QT, Stack, [Char, Char | Tail]) when QT == line, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == semi, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ifok, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ampi, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ifnz, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == pipe, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == back, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == pren, Char == "&" -> close_context(ifok, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "&" -> close_context(ampi, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char, Char | Tail]) when QT == line, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == semi, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ifok, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ampi, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == ifnz, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == pipe, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == back, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char, Char | Tail]) when QT == pren, Char == "|" -> close_context(ifnz, [{context, QT}] ++ Stack, Tail);

parse_context(QT, Stack, [Char | Tail]) when QT == line, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == semi, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifok, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ampi, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == ifnz, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pipe, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == back, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);
parse_context(QT, Stack, [Char | Tail]) when QT == pren, Char == "|" -> close_context(pipe, [{context, QT}] ++ Stack, Tail);

parse_context(escp, _Stack, ["\n"]) -> throw({context, escp}); 
parse_context(escp, Stack, [[] | Tail]) -> parse_context(escp, Stack, Tail);
parse_context(escp, [Type | Stack], [Head | Tail]) -> 
	[First | Rest] = Head,
	Escape = [First, {close_context, escp}],
	{NewTail, Stack} = parse(Type, Stack, [Rest] ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Stack};

parse_context(dbcp, _Stack, ["\n"]) -> throw({context, escp});
parse_context(dbcp, Stack, [[] | Tail]) -> parse_context(dbcp, Stack, Tail);
parse_context(dbcp, [Type | Stack], [Head | Tail]) -> 
	[First | Rest] = Head,

	case [First] of 
		"\$"	-> Escape = [[First], {close_context, dbcp}], Left = [Rest];
		"\`"	-> Escape = [[First], {close_context, dbcp}], Left = [Rest];
		"\""	-> Escape = [[First], {close_context, dbcp}], Left = [Rest];
		"\\"	-> Escape = [[First], {close_context, dbcp}], Left = [Rest]; 
		"\n"	-> Escape = throw({context, escp}), Left = [Rest]; 
		_Other	-> Escape = [{close_context, dbcp}], Left = [[First] ++ Rest]
	end,  
	{NewTail, Stack} = parse(Type, Stack, Left ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Stack};

parse_context(QType, Stack, [Head | Tail]) -> 
	{NewTail, _ReturnStack} = parse({context, QType}, Stack, Tail), 
	{[Head] ++ NewTail, Stack}.
	