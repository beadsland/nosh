% REF: http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_05

% TODO: Eclipse permission denied errors
% TODO: payments


% TODO: git
% TODO: version macros from git

% grouping reference for offline development...


% brooklyn
% lunch meat
% walk
% email


% TODO: Grouping - http://sayle.net/book/basics.htm#grouping_commands
% TODO: Tokenizing
% TODO: conservative module loader
% TODO: Executing
% TODO: $ - Parameter expansion
% TODO: ${} - Parameter expansion
% TODO: $() - Command substitution
% TODO: $(()) - Arithmetic Expansion
% TODO: Field splitting
% TODO: Reserved words
% TODO: Alias substitution
% TODO: Here document
% TODO: Etc., etc. 

-module(command).

-export([version/0]).
-export([eval/4]).

version() -> Version = "0.0.16", Version. 

eval(Subject, _Stdin, _Stdout, Stderr) -> parse(Subject, Stderr).

stderr(Stderr, Format, List) -> Stderr ! {self(), stderr, io_lib:format(Format, List)}.


% Parser only tokenizes on matching quotes thus far.
parse(Subject, Stderr) ->
	{ok, MP} = re:compile("([\\\\\"\'\`\n])"), 
	
	Split = re:split(Subject, MP, [{return, list}]),
	Pred = fun(T) -> case T of [] -> false; _Else -> true end end,
	CleanSplit = lists:filter(Pred, Split), 
	
	QuoteErr = "Quote error: Closing ~s missing~n", 
	try parse(eval, [], CleanSplit) of
		Result -> Result
	catch
		{eval, eval}  	-> stderr(Stderr, "Eval error: shouldn't happen~n", []), {failed};
		{quote, line} 	-> stderr(Stderr, QuoteErr, ["EOL"]), {failed};
		{quote, back} 	-> stderr(Stderr, QuoteErr, ["\`"]), {failed};
		{quote, doub} 	-> stderr(Stderr, QuoteErr, ["\""]), {failed};
		{quote, sing} 	-> stderr(Stderr, QuoteErr, ["\'"]), {failed};
		{quote, escp} 	-> stderr(Stderr, "Quote error: Line continuation not supported~n", []), {failed}
	end. 

% Unwind parse stream.
parse(eval, [], []) -> {[close_eval], []};
parse(eval, [], Split) -> 
	{Parse, [eval]} = close_quote(line, [eval], Split),
	[{{quote, line}, Tokens}, close_eval] = Parse, 
	Tokens;
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

% 	if this never errors out, then we don't need to pass Context in tuple/2
%		{Head, Context, {_Type, Tail}}	-> {Head ++ Tail, Context}; 

		{Head, ReturnContext}			-> {Head, ReturnContext}
	end.  

% Wind up quote block.
close_quote(QType, Context, List) ->
	{Tail, _TailContext} = parse({quote, QType}, Context, List), 
	Close = {close_quote, QType},
	Pred = fun(T) -> T /= Close end,
	{L1, L2} = lists:splitwith(Pred, Tail),
	Quote = if
		QType == dbcp, L1 == []	-> "\\";
		true					-> {{quote, QType}, L1}
	end,
	{[Quote] ++ lists:delete(Close, L2), Context}.

% Unwind quote stream. 
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
	