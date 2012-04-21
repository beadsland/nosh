%% @doc This is a preliminary draft of the command line parser for <code>nosh</code>.
%% @author Beads D. Land-Trujillo <beads.d.land@gmail.com>
%% @copyright 2012 Beads D. Land-Trujillo
%% @reference See <a href="http://sayle.net/book/basics.htm">Shell Basics</a> for overview of functionality.  (to be implemented)
%% @reference See <a href="http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html">Shell Command Language</a> 
%% for detailed specification. (to be implemented)
%% @end

%$ TODO: follow up bug report on Erlide permission denied errors
%% TODO: edocs all modules
%% TODO: Grouping - http://sayle.net/book/basics.htm#grouping_commands
%% TODO: Tokenizing
%% TODO: $ - Parameter expansion
%% TODO: ${} - Parameter expansion
%% TODO: $() - Command substitution
%% TODO: $(()) - Arithmetic Expansion
%% TODO: Field splitting
%% TODO: Reserved words
%% TODO: conservative module loader
%% TODO: Executing
%% TODO: Alias substitution
%% TODO: Line continuation
%% TODO: Here document
%% TODO: Etc., etc. 

%% @version 0.1.1
-module(command).
-export([eval/4]).
-define(STDERR(Stderr, Format, List), Stderr ! {self(), stderr, io_lib:format(Format, List)}).

%% @doc Return author's revision number of this module.  Used for debugging purposes.
-export([version/0]).
-spec version()-> nonempty_string().
%%
version() -> Version = "0.1.1", Version. 


%% @doc Parse command line string and return a list of nested quoting and grouping context blocks, 
%% or else <code>failed</code> on a caught syntax exception.
%%
%% This is a temporary function name, pending refactoring to reflect full execution of parsed command lines.
%% @end
-type io_proc() :: pid().
-type quote_type() :: line | back | doub | sing | escp | dbcp.
-type context_type() :: {eval, eval} | {quote, quote_type()}.
-type block() :: nonempty_string() | {context_type(), list(block())}.
-spec eval(Subject :: nonempty_string(), Stdin :: io_proc(), Stdout :: io_proc(), Stderr :: io_proc()) -> failed | list(block()).
%%
eval(Subject, _Stdin, _Stdout, Stderr) -> parse(Subject, Stderr).



%% Parse command line string and return a list of nested quoting and grouping contexts.
%$ Handle thrown errors for unmatched quoting and grouping characters.
parse(Subject, Stderr) ->
	{ok, MP} = re:compile("([\\\\\"\'\`\n])"), 
	
	Split = re:split(Subject, MP, [{return, list}]),
	Pred = fun(T) -> case T of [] -> false; _Else -> true end end,
	CleanSplit = lists:filter(Pred, Split), 
	
	QuoteErr = "Quote error: Closing ~s missing~n", 
	try close_quote(line, [eval], CleanSplit) of
		{Parse, [eval]} -> [{{quote, line}, ContextList}, close_eval] = Parse, ContextList	
	catch
		{eval, eval}  	-> ?STDERR(Stderr, "Eval error: shouldn't happen~n", []), failed;
		{quote, line} 	-> ?STDERR(Stderr, QuoteErr, ["EOL"]), failed;
		{quote, back} 	-> ?STDERR(Stderr, QuoteErr, ["\`"]), failed;
		{quote, doub} 	-> ?STDERR(Stderr, QuoteErr, ["\""]), failed;
		{quote, sing} 	-> ?STDERR(Stderr, QuoteErr, ["\'"]), failed;
		{quote, escp} 	-> ?STDERR(Stderr, "Quote error: Line continuation not supported~n", []), failed
	end. 


%% Parse list of strings split on quoting and grouping characters, according to current context type.
%% Return tuple of block list and context stack OR tuple of 'close_quote', context stack, and trailing context tree.
%% Throw exception for unmatched quoting or grouping character.
parse(eval, [], []) -> {[close_eval], []};
parse(Type, _Context, []) -> throw(Type);
parse(Type, Context, [[] | Tail]) -> parse(Type, Context, Tail);
parse(Type, Context, [Head | Tail]) when is_number(Head) ->
	HeadStr = io_lib:format([Head], []),  
	parse(Type, Context, HeadStr ++ Tail);
parse({quote, QType}, Context, List) -> 
	io:format("parse_quote(~p, ~p, ~p)~n", [QType, Context, List]), 	
	Parse = parse_quote(QType, Context, List), 
	io:format("~nparse_quote(~p, ~p, ~p) ->~n     ~p~n", [QType, Context, List, Parse]), 
										 
	case Parse of 							
		{close_quote, Context, Tail}	-> Close = {close_quote, QType},
										   [SuperType | SuperContext] = Context,
										   {Post, _PCon} = parse(SuperType, SuperContext, Tail),
										   {[Close] ++ Post, SuperContext};
		{Tail, ReturnContext}			-> {Tail, ReturnContext}
	end.  

%% Wind up quote block.
close_quote(QType, Context, List) ->
	{Tail, _TailContext} = parse({quote, QType}, Context, List), 
	Close = {close_quote, QType},
	Pred = fun(T) -> T /= Close end,
	{L1, L2} = lists:splitwith(Pred, Tail),
	Quote = if
		QType == dbcp, L1 == []	-> "\\";      % Didn't escape anything, so restore backslash as regular character.
		true					-> {{quote, QType}, L1}
	end,
	{[Quote] ++ lists:delete(Close, L2), Context}.

%% Unwind quote stream. 
parse_quote(line, Context, ["\n" | Tail]) -> {close_quote, Context, Tail};

parse_quote(line, Context, ["\`" | Tail]) -> close_quote(back, [{quote, line}] ++ Context, Tail);
parse_quote(doub, Context, ["\`" | Tail]) -> close_quote(back, [{quote, doub}] ++ Context, Tail);
parse_quote(back, Context, ["\`" | Tail]) -> {close_quote, Context, Tail};

parse_quote(line, Context, ["\"" | Tail]) -> close_quote(doub, [{quote, line}] ++ Context, Tail);
parse_quote(back, Context, ["\"" | Tail]) -> close_quote(doub, [{quote, back}] ++ Context, Tail);
parse_quote(doub, Context, ["\"" | Tail]) -> {close_quote, Context, Tail};

parse_quote(line, Context, ["\'" | Tail]) -> close_quote(sing, [{quote, line}] ++ Context, Tail);
parse_quote(back, Context, ["\'" | Tail]) -> close_quote(sing, [{quote, back}] ++ Context, Tail);
parse_quote(sing, Context, ["\'" | Tail]) -> {close_quote, Context, Tail};

parse_quote(line, Context, ["\\" | Tail]) -> close_quote(escp, [{quote, line}] ++ Context, Tail);
parse_quote(doub, Context, ["\\" | Tail]) -> close_quote(dbcp, [{quote, doub}] ++ Context, Tail);
parse_quote(back, Context, ["\\" | Tail]) -> close_quote(escp, [{quote, back}] ++ Context, Tail);

parse_quote(escp, _Context, ["\n"]) -> throw({quote, escp}); 
parse_quote(escp, Context, [[] | Tail]) -> parse_quote(escp, Context, Tail);
parse_quote(escp, [Type | Context], [Head | Tail]) -> 
	[First | Rest] = Head,
	Escape = [First, {close_quote, escp}],
	{NewTail, _TailContext} = parse(Type, Context, [Rest] ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Context};

parse_quote(dbcp, _Context, ["\n"]) -> throw({quote, escp});
parse_quote(dbcp, Context, [[] | Tail]) -> parse_quote(dbcp, Context, Tail);
parse_quote(dbcp, [Type | Context], [Head | Tail]) -> 
	[First | Rest] = Head,

	case [First] of
		"\$"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\'"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\""	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest];
		"\\"	-> Escape = [[First], {close_quote, dbcp}], Left = [Rest]; 
		_Other	-> Escape = [{close_quote, dbcp}], Left = [[First] ++ Rest]
	end,  
	{NewTail, _TailContext} = parse(Type, Context, Left ++ Tail), 
	{Escape ++ NewTail, [Type] ++ Context};

parse_quote(QType, Context, [Head | Tail]) -> 
	io:format("+parse_quote(~p, ~p, ~p)~n", [QType, Context, [Head] ++ Tail]),  
	{NewTail, TailContext} = parse({quote, QType}, Context, Tail), 
	io:format("+parse_quote(~p, ~p, ~p) ->~n     ~p~n", [QType, Context, [Head] ++ Tail, {NewTail, TailContext}]), 
	{[Head] ++ NewTail, Context}.
	