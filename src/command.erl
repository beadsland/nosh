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

%% TODO: figure out module name clash strategy
%% TODO: revise loops to use fully-qualified function calls
%% TODO: doc update:  "procedural" should read "imperative"
%% TODO: doc update:  first character, lower case:  atom; upper case:  variable
%% TODO: Field splitting/Tokenizing
%% TODO: rebar dependency directory structure
%% TODO: built-in functions dependency
%% TODO: $PATH search
%% TODO: Executing - implicit start
%% TODO: Executing - explicit function
%% TODO: Executing - getoptions functions
%% TODO: Tilde expansion
%% TODO: Reserved words
%% TODO: $ - Parameter expansion
%% TODO: ${...} - Parameter expansion
%% TODO: $(...) - Command substitution
%% TODO: $((...)) - Arithmetic Expansion
%% TODO: [...] Lists
%% TODO: [`'...`'] Lists command substituion
%% TODO: (...) second position parameter Lists
%% TODO: {...} Tuples
%% TODO: `<...>' Pids
%% TODO: `<<...>>' Bitstrings
%% TODO: Fileglobs
%% TODO: Redirection
%% TODO: Here document
%% TODO: conservative module loader
%% TODO: Alias substitution
%% TODO: Line continuation
%% TODO: Etc., etc. 


%% @version 0.1.2
-module(command).
-export([eval/4]).

%% @doc Return author's revision number of this module.  Used for debugging purposes.
-export([version/0]).
-spec version()-> nonempty_string().
%%
version() -> Version = "0.1.2", Version. 


%% @doc Parse command line string and return a list of nested quoting and grouping context blocks, 
%% or else <code>failed</code> on a caught syntax exception.
%%
%% This is a temporary function name, pending refactoring to reflect full execution of parsed command lines.
%% @end
-type io_proc() :: pid().
-type quote_type() :: back | doub | sing | escp | dbcp.
-type group_type() :: line | pren | ifok | ambi | ifnz | pipe.
-type context_type() :: {eval, eval} | {quote, group_type()} | {quote, quote_type()}.
-type block() :: nonempty_string() | {context_type(), list(block())}.
-spec eval(Subject :: nonempty_string(), Stdin :: io_proc(), Stdout :: io_proc(), Stderr :: io_proc()) -> failed | list(block()).
%%
eval(Subject, _Stdin, _Stdout, Stderr) -> parse(Subject, Stderr).

-define(QUOTE_CHARS, "\\\\\"\'\`\n").
-define(GROUP_CHARS, "\;\(\)\&\|").     % curly braces are reserved words, not grouping characters
-define(SPACE_CHARS, "\ \t\n").
-define(STDERR(Format, List), Stderr ! {self(), stderr, io_lib:format(Format, List)}).

%% Parse command line string and return a list of nested quoting and grouping contexts.
%$ Handle thrown errors for unmatched quoting and grouping characters.
parse(Subject, Stderr) ->
	Pattern = io_lib:format("([~s~s~s])", [?QUOTE_CHARS, ?GROUP_CHARS, ?SPACE_CHARS]),
	{ok, MP} = re:compile(Pattern), 
	
	Split = re:split(Subject, MP, [{return, list}]),
	Pred = fun(T) -> case T of [] -> false; _Else -> true end end,
	CleanSplit = lists:filter(Pred, Split), 
	
	QuoteErr = "Quote error: Closing ~s missing~n", 
	GroupErr = "Group error: Closing ~s missing~n", 
	try close_quote(line, [eval], CleanSplit) of
		{Parse, [eval]} -> [{{quote, line}, ContextList}, close_eval] = Parse, ContextList	
	catch
		{eval, eval}  	-> ?STDERR("Eval error: shouldn't happen~n", []), failed;
		{quote, line} 	-> ?STDERR(GroupErr, ["EOL"]), failed;
		{quote, semi}	-> ?STDERR(GroupErr, ["EOL"]), failed;
		{quote, pren}	-> ?STDERR(GroupErr, ["\)"]), failed;
		{close, pren}	-> ?STDERR("Group error: Unmatched closing parentheses~n", []), failed;
		{quote, back} 	-> ?STDERR(QuoteErr, ["\`"]), failed;
		{quote, doub} 	-> ?STDERR(QuoteErr, ["\""]), failed;
		{quote, sing} 	-> ?STDERR(QuoteErr, ["\'"]), failed;
		{quote, escp} 	-> ?STDERR("Quote error: Line continuation not supported~n", []), failed
	end. 


%% Parse list of strings split on quoting and grouping characters, according to current context type.
%% Return tuple of block list and context stack OR tuple of 'close_quote', context stack, and trailing context tree.
%% Throw exception for unmatched quoting or grouping character.
parse(eval, [], []) -> {[close_eval], []};
parse(Type, _Context, []) -> throw(Type);
parse(Type, Context, [[] | Tail]) -> parse(Type, Context, Tail);
parse(Type, Context, [Head | Tail]) when is_integer(Head) ->
	HeadStr = io_lib:format([Head], []),  
	parse(Type, Context, HeadStr ++ Tail);
parse({quote, QType}, Context, List) -> 	
	io:format("parse_quote(~p, ~p, ~p)~n", [QType, Context, List]), 	
	Parse = parse_quote(QType, Context, List), 
	io:format("~nparse_quote(~p, ~p, ~p) ->~n     ~p~n", [QType, Context, List, Parse]), 
										 
	case Parse of 							
		{close_quote, Context, Tail}	-> Close = {close_quote, QType},
										   [SuperType | SuperContext] = Context,
										   {Post, _ReturnContext} = parse(SuperType, SuperContext, Tail),
										   {[Close] ++ Post, SuperContext};
		{Tail, ReturnContext}			-> {Tail, ReturnContext}
	end.  

%% Wind up quote block.
close_quote(QType, Context, List) ->
	io:format("Xparse_quote(~p, ~p, ~p)~n", [QType, Context, List]), 	
	{Tail, _ReturnContext} = parse({quote, QType}, Context, List), 
	Close = {close_quote, QType},
	Pred = fun(T) -> T /= Close end,
	{L1, L2} = lists:splitwith(Pred, Tail),
	if
		QType == dbcp, L1 == []	-> Quote = "\\";      % Didn't escape anything, so restore backslash as regular character.
		true					-> Quote = {{quote, QType}, L1}
	end,
	{[Quote] ++ lists:delete(Close, L2), Context}.


%% @doc Unwind quote and group stream.
parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\n" -> {close_quote, Context, Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\n" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\n" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\n" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\n" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\n" -> {close_quote, Context, [Char] ++ Tail};

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\;" -> close_quote(semi, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\"" -> close_quote(doub, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == doub, Char == "\"" -> {close_quote, Context, Tail};

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\\" -> close_quote(escp, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == doub, Char == "\\" -> close_quote(dbcp, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\'" -> close_quote(sing, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == sing, Char == "\'" -> {close_quote, Context, Tail};

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == doub, Char == "\`" -> close_quote(back, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\`" -> {close_quote, Context, Tail};
  
parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\(" -> close_quote(pren, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "\)" -> {close_quote, Context, Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "\)" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "\)" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "\)" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "\)" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "\)" -> {close_quote, Context, [Char] ++ Tail};
parse_quote(_QT, _Context, [Char | _Tail]) when Char == "\)" -> throw({close, pren});
 
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == line, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == semi, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ifok, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ampi, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ifnz, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == pipe, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == back, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == pren, Char == "&" -> close_quote(ifok, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "&" -> close_quote(ampi, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == line, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == semi, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ifok, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ampi, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == ifnz, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == pipe, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == back, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | [Char | Tail]]) when QT == pren, Char == "|" -> close_quote(ifnz, [{quote, QT}] ++ Context, Tail);

parse_quote(QT, Context, [Char | Tail]) when QT == line, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == semi, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifok, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ampi, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == ifnz, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pipe, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == back, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);
parse_quote(QT, Context, [Char | Tail]) when QT == pren, Char == "|" -> close_quote(pipe, [{quote, QT}] ++ Context, Tail);

parse_quote(escp, _Context, ["\n"]) -> throw({quote, escp}); 
parse_quote(escp, Context, [[] | Tail]) -> parse_quote(escp, Context, Tail);
parse_quote(escp, [Type | Context], [Head | Tail]) -> 
	[First | Rest] = Head,
	Escape = [First, {close_quote, escp}],
	{NewTail, Context} = parse(Type, Context, [Rest] ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Context};

parse_quote(dbcp, _Context, ["\n"]) -> throw({quote, escp});
parse_quote(dbcp, Context, [[] | Tail]) -> parse_quote(dbcp, Context, Tail);
parse_quote(dbcp, [Type | Context], [Head | Tail]) -> 
	[First | Rest] = Head,

	case [First] of 
		"\$"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\`"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\""	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\\"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest]; 
		"\n"	-> Escape = throw({quote, escp}), Left = [Rest]; 
		_Other	-> Escape = [{close_quote, dbcp}], Left = [[First] ++ Rest]
	end,  
	{NewTail, Context} = parse(Type, Context, Left ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Context};

parse_quote(QType, Context, [Head | Tail]) -> 
	{NewTail, _ReturnContext} = parse({quote, QType}, Context, Tail), 
	{[Head] ++ NewTail, Context}.
	