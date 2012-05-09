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
%%
%% Each command line is decomposed into a context tree, representing
%% Execution, Grouping, Quoting, Substitution and Term contexts.
%%
%% <i>Not all parsing rules described below have been implemented.</i>
%%
%% <ul>
%% <li> {@section Execution Contexts} </li>
%% <li> {@section Grouping Contexts} </li>
%% <li> {@section Quoting Contexts} </li>
%% <li> {@section Substitution Contexts} </li>
%% <li> {@section Term Contexts} </li>
%% </ul>
%%
%% == Execution Contexts ==
%%
%% The default context of all command lines is Bourne Context, and all
%% grouping, quoting, and substitution contexts are parsed in accordance
%% with Bourne shell syntax.
%%
%% <i>The below discussion of Erlang Context describes functionality
%% yet to be implemented.</i>
%%
%% === Erlang Context ===
%%
%% The Erlang Context is invoked with a left-wise conjunction,
%% period (`.'), which causes all words and subcontexts within the current
%% context to be interpreted according to Erlang syntax.  Erlang Context
%% sequences may be enclosed arbitrarily within grouping contexts.
%% For example:
%%
%% <code>&gt; PARAM='value'; Result = my_mod:my_func($PARAM). &amp;&amp;
%% echo Success</code>
%%
%% This results in a tree of nested contexts that would be evaluated first
%% as an environment variable assignment in Bourne Context, followed by an
%% Erlang Context function call (receiving a parameter by Bourne Context
%% parameter expansion) and single-assignment variable match, and finally
%% followed by an echo command in Bourne Context if and only if the
%% previous, Erlang Context, statement returns the equivalent of a Bourne
%% non-zero exit code (either an Erlang `ok' atom or `{ok, ...}' tuple).
%%
%% === Grouping Contexts ===
%%
%% Grouping is parsed for entire line prior to evaluation and execution,
%% unlike Bourne standard.  Thus transactional integrity is preserved:
%% command execution only occurs if entire command sequence parses
%% correctly.
%%
%% The parentheses context is parsed without respect to its function
%% either as a Bourne subshell grouping or an Erlang function parameter
%% list (this distinction being left to the evaluation step.)
%%
%% The faux within-shell grouping syntax of Bourne shell
%% (`{ ...; };')--implemented with the open curly bracket (`{') and closed
%% curly bracket (`}') as reserved words--is not supported by `nosh',
%% curly brackets instead marking {@section Tuples}.
%%
%% The semicolon (`;') conjunction works in `nosh' as it does in Bourne
%% shell, execution of the command group following the semicolon being
%% deferred until the command group preceding the semicolon has completed.
%%
%% Likewise, double ampersand (`&&') and double vertical bar (`||') afford
%% conditional execution.  The command group following a double ampersand
%% executing only on a zero status (or an Erlang return value of `ok' or
%% `{ok, ...}'), and the command group following a double vertical bar
%% executing only on a non-zero status (any other Erlang return value).
%%
%% These {@section Bourne Context} conjunctions are parsed right-wise.
%% That is, everything that follows the conjunction is returned as a
%% subcontext in the last element of the enclosing context.
%%
%% There are two left-wise conjunctions, the period (`.'), marking an
%% {@section Erlang Context}, and the `<newline>', marking the top-level
%% command line context.
%%
%% === Pipes ===
%%
%% As per Bourne shell, pipes are marked by a single vertical bar (`|')
%% conjunction.  Erlang processes may participate in pipe relationships if
%% they implement the Nosh_exec Behaviour.  Otherwise, processes
%% grouped by the single vertical bar will run, but each in an isolated
%% subshell environment without any access to piped standard input and
%% output streams.
%%
%% === Background Jobs ===
%%
%% The single ampersand (`&') conjunction marks the preceding command
%% group as a background job as per `bash' (Bourne Again Shell) syntax.
%%
%% === Quoting Contexts ===
%%
%% Single quoted (`` '...' ''), double quoted (`"..."'), and back quoted
%% (<code>`'...`'</code>) character sequences are supported, as per the
%% Bourne shell, as is use of the backslash (`\') to escape special
%% characters.  Additionally, single and double quoted sequences have
%% special meaning in {@section Erlang Context} and when passed as
%% Erlang command parameters.  (Bourne-standard quote removal does not
%% occur in Erlang Context or for Erlang command parameters.)
%%
%% Single quotes remove the special meaning of all characters they enclose.
%% In addition, single quotes mark the `atom()' type in Erlang Context and
%% when passed to Erlang functions.
%%
%% Double quotes remove the special meaning of all characters other than
%% the dollar sign (`$'), the back quote (<code>`'</code>), the
%% backslash (`\'), and the newline (per {@section Multiline Parsing}).
%% Additionally, double quotes indicate the `string()' type, <i>i.e.</i>
%% a `list()' of characters, in Erlang Context and when passed as Erlang
%% command parameters.
%%
%% Strings are passed to Erlang functions only after all embedded
%% {@section Substitution Contexts} have been evaluated.
%%
%% Back quotes mark {@section Command Substitution}, the back quoted
%% character sequence being replaced by the results of the substituted
%% command(s) execution.
%%
%% The {@section Lists} back quote (<code>[`'...`']</code>) and
%% {@section Tuples} back quote (<code>{`'...`'}</code>) are two special
%% constructs for passing command substitution results as
%% Erlang-compatible {@section Term Contexts} rathern than
%% Bourne-standard whitespace delimited {@section Words}.
%%
%% The backslash character (`\') operates to escape the following
%% character, as per both Bourne and Erlang syntax.
%%
%% === Multiline Parsing ===
%%
%% Presently, multi-line parsing is not supported.  Any line ending in a
%% backslash (an escaped carriage return) will result in an error.
%%
%% == Substitution Contexts ==
%%
%% === Parameter Expansion ===
%%
%% <i>Not yet implemented.</i>
%%
%% === Command Substitution ===
%%
%% <i>Not yet implemented</i>.
%%
%% === Arithmetic Expansion ===
%%
%% <i>Not yet implemented</i>.
%%
%% == Term Contexts ==
%%
%% === Words ===
%%
%% <i>Not yet implemented</i>.
%%
%% The simplest `nosh' command line consists of a command followed by zero
%% or more word contexts, each being delimited by the characters in the
%% `IFS' environment variable (or `<space>', `<tab>' and `<newline>'
%% if `IFS' is undefined).  Each single and/or double quoted context is
%% treated as a word for this purpose (see {@section Quoting Contexts}).
%%
%% Unquoted (<i>i.e.</i> bare) words that begin with a lower case
%% character and single quote contexts are passed to Erlang functions as
%% type `atom()', while double quote contexts are passed as type `string()'.
%%
%% Bare words that begin with an upper case character are treated as
%% Erlang single-assignment variables as any such variables are defined
%% in the current shell.  (See also {@section Parameter Expansion})
%%
%% === Lists ===
%%
%% <i>Not yet implemented</i>.
%%
%% Zero or more Term contexts are marked as within a single List context
%% (not to be confused with a {@section Parameter List}) when enclosed by
%% an open square bracket (`[') and close square bracket (`]').  Per
%% Erlang syntax, elements of a List are delimited by commas (`,'),
%% whitespace is ignored, and `IFS' specified delimiters have no special
%% meaning.
%%
%% {@section Command Substitution} may be used to generate an Erlang List,
%% through use of the Lists back quote construct (<code>[`'...`']</code>).
%%
%% ==== Function Parameter List ====
%%
%% <i>Not yet implemented</i>.
%%
%% A Function Parameter List follows Erlang syntax for specifying the
%% parameters (and indirectly, the arity) of a function, and consists of
%% zero or more comma-delimited Term contexts enclosed by an
%% open parentheses (`(') and close parentheses (`)') rather than square
%% brackets.  A Parameter List may only appear in second position after a
%% command, and only in explicit function mode (see {@link nosh_exec}.
%%
%% In first position, parenthese denote a {@section Grouping Context}.
%%
%% === Tuples ===
%%
%% <i>Not yet implemented</i>.
%%
%% One or more Term contexts are marked as within a single Tuple
%% context when enclosed by an open curly bracket (`{') and close curly
%% bracket (`}').  Per Erlang syntax, elements of a Tuple are delimited by
%% commas (`,'), whitespace is ignored, and `$IFS' specified delimiters
%% have no special meaning.
%%
%% {@section Command Substitution} may be used to generate an Erlang Tuple,
%% through use of the Tuples back quote construct (<code>{`'...`'}</code>).
%%
%% === Pids ===
%%
%% <i>Not yet implemented</i>.
%%
%% === Bitstrings ===
%%
%% <i>Not yet implemented</i>.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo
%% @reference See <a href="http://sayle.net/book/basics.htm">Shell
%% Basics</a> for overview of functionality.  (to be implemented)
%% @end
%% @reference See <a href="http://goo.gl/edyre">Shell Command Language</a>
%% for detailed specification. (to be implemented)
%% @end

%% TODO: Tokenizing
%% TODO: refactor documentation to eval/exec/etc.
%% TODO: Erlang Stack
%% TODO: Line continuation

%% @version 0.1.6
-module(nosh_parse).
-version("0.1.6").

%%
%% Include files
%%

-define(debug, true).
-include("macro.hrl").

-define(QUOTE_CHARS, "\\\\\"\'\`\n").
-define(GROUP_CHARS, "\;\(\)\&\|").
-define(SPACE_CHARS, "\ \t\n").

%%
%% Exported functions
%%

-export([parse/2, parse/3]).

%%
%% API functions
%%

%% @doc Parse command line string and return a list of nested contexts,
%% or else `failed' on a caught syntax exception.
%%
%% Handle thrown errors for unmatched quoting, grouping, and term context
%% symbols.
%% @end
-type term_type() :: word | list | plst | tupl | epid | bstr.
-type quote_type() :: line | back | doub | sing | escp | dbcp.
-type group_type() :: pren | semi | ifok | ambi | ifnz | pipe.
-type exec_type() :: brne | erln.
-type context_type() :: term_type() | quote_type()
                        | group_type() | exec_type().
-type context_desc() :: {context, context_type()}.
-type context_list() :: [context()].
-type context() :: nonempty_string() | {context_desc(), context_list()}.
-type parse_error_type() :: quote | group.
-type parse_error() :: {parse_error_type(), string()}.
-spec parse(IO :: #std{}, Subject :: nonempty_string()) ->
      {ok, context_list()} | {error, parse_error()}.
%%
parse(IO, Subject) ->
  ?INIT_DEBUG,
  Pattern = io_lib:format("([~s~s~s])",
              [?QUOTE_CHARS, ?GROUP_CHARS, ?SPACE_CHARS]),
  {ok, MP} = re:compile(Pattern),

  Split = re:split(Subject, MP, [{return, list}]),
  Pred = fun(T) -> case T of [] -> false; _Else -> true end end,
  CleanSplit = lists:filter(Pred, Split),

  try_symbols(CleanSplit).


%% @doc Parse list of strings split on quoting and grouping characters,
%% according to current Stack type.  Return tuple of context tree, context
%% stack and unparsed tail OR tuple of 'close_context', context stack, and
%% trailing context tree.
%%
%% Throw exception for unmatched quoting or grouping character.
%% @end
%% @todo spec this function
parse({context, brne}, [], []) -> {[{close_context, brne}], []};
parse({context, brne}, [], List) ->
    nosh_context:close_context(line, [{context, brne}], List);
parse(Type, _Stack, []) -> throw(Type);
parse(Type, Stack, [[] | Tail]) -> parse(Type, Stack, Tail);
parse(Type, Stack, [Head | Tail]) when is_integer(Head) ->
  HeadStr = io_lib:format([Head], []),
  parse(Type, Stack, [HeadStr | Tail]);
parse({context, QType}, Stack, List) ->
  ?DEBUG("parse_context(~p, ~p, ~p)~n", [QType, Stack, List]),
  Parse = nosh_context:parse_context(QType, Stack, List),
  ?DEBUG("parse_context(~p, ~p, ~p) ->~n     ~p~n",
       [QType, Stack, List, Parse]),
  case Parse of
    {close_context, Stack, Tail}    ->
      parse_close(QType, Stack, Tail);
    {Tail, ReturnStack}	            ->
      {Tail, ReturnStack}
  end.

%%
%% Local functions
%%

% close_context clause of parse/3
parse_close(QType, Stack, Tail) ->
    Close = {close_context, QType},
    [SuperType | SuperStack] = Stack,
    {Post, _ReturnStack} = parse(SuperType, SuperStack, Tail),
    {[Close | Post], SuperStack}.

-define(CLOSING(T), io_lib:format("closing ~s missing", [T])).

try_symbols(Symbols) ->
  Stack = [],
  try nosh_context:close_context(brne, Stack, Symbols) of
    {List, Stack} 		-> [{{context, brne}, Line}] = List,
                           [{{context, line}, Terms}] = Line,
                           {ok, Terms}
  catch
    {context, line} 	-> {error, {quote, ?CLOSING("EOL")}};
    {context, back} 	-> {error, {quote, ?CLOSING("\`")}};
    {context, doub} 	-> {error, {quote, ?CLOSING("\"")}};
    {context, sing} 	-> {error, {quote, ?CLOSING("\'")}};
    {context, escp} 	-> {error, {quote, "line continuation not supported"}};
    {context, pren}		-> {error, {group, ?CLOSING("\)")}};
    {close, pren}		-> {error, {group, "unmatched closing parentheses"}}
  end.