

#Module nosh_parse#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This is a preliminary draft of the command line parser for `nosh`.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.6

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__* See [Shell
Basics](http://sayle.net/book/basics.htm) for overview of functionality.  (to be implemented)
* See [Shell Command Language](http://goo.gl/edyre)
for detailed specification. (to be implemented)


__<font color="red">To do</font>__
<br></br>
* <font color="red"> Tokenizing</font>
* <font color="red"> refactor documentation to eval/exec/etc.</font>
* <font color="red"> Erlang Stack</font>
* <font color="red"> Line continuation</font>
<a name="description"></a>

##Description##




Each command line is decomposed into a context tree, representing
Execution, Grouping, Quoting, Substitution and Term contexts.



_Not all parsing rules described below have been implemented._


* [Execution Contexts](#Execution_Contexts)

* [Grouping Contexts](#Grouping_Contexts)

* [Quoting Contexts](#Quoting_Contexts)

* [Substitution Contexts](#Substitution_Contexts)

* [Term Contexts](#Term_Contexts)





###<a name="Execution_Contexts">Execution Contexts</a>##




The default context of all command lines is Bourne Context, and all
grouping, quoting, and substitution contexts are parsed in accordance
with Bourne shell syntax.



_The below discussion of Erlang Context describes functionalityyet to be implemented._



####<a name="Erlang_Context">Erlang Context</a>##




The Erlang Context is invoked with a left-wise conjunction,
period (`.`), which causes all words and subcontexts within the current
context to be interpreted according to Erlang syntax.  Erlang Context
sequences may be enclosed arbitrarily within grouping contexts.
For example:



`> PARAM='value'; Result = my_mod:my_func($PARAM). &&
echo Success`



This results in a tree of nested contexts that would be evaluated first
as an environment variable assignment in Bourne Context, followed by an
Erlang Context function call (receiving a parameter by Bourne Context
parameter expansion) and single-assignment variable match, and finally
followed by an echo command in Bourne Context if and only if the
previous, Erlang Context, statement returns the equivalent of a Bourne
non-zero exit code (either an Erlang `ok` atom or `{ok, ...}` tuple).



####<a name="Grouping_Contexts">Grouping Contexts</a>##




Grouping is parsed for entire line prior to evaluation and execution,
unlike Bourne standard.  Thus transactional integrity is preserved:
command execution only occurs if entire command sequence parses
correctly.



The parentheses context is parsed without respect to its function
either as a Bourne subshell grouping or an Erlang function parameter
list (this distinction being left to the evaluation step.)



The faux within-shell grouping syntax of Bourne shell
(`{ ...; };`)--implemented with the open curly bracket (`{`) and closed
curly bracket (`}`) as reserved words--is not supported by `nosh`,
curly brackets instead marking [Tuples](#Tuples).



The semicolon (`;`) conjunction works in `nosh` as it does in Bourne
shell, execution of the command group following the semicolon being
deferred until the command group preceding the semicolon has completed.



Likewise, double ampersand (`&&`) and double vertical bar (`||`) afford
conditional execution.  The command group following a double ampersand
executing only on a zero status (or an Erlang return value of `ok` or
`{ok, ...}`), and the command group following a double vertical bar
executing only on a non-zero status (any other Erlang return value).



These [Bourne Context](#Bourne_Context) conjunctions are parsed right-wise.
That is, everything that follows the conjunction is returned as a
subcontext in the last element of the enclosing context.



There are two left-wise conjunctions, the period (`.`), marking an
[Erlang Context](#Erlang_Context), and the `<newline>`, marking the top-level
command line context.



####<a name="Pipes">Pipes</a>##




As per Bourne shell, pipes are marked by a single vertical bar (`|`)
conjunction.  Erlang processes may participate in pipe relationships if
they implement the Nosh_exec Behaviour.  Otherwise, processes
grouped by the single vertical bar will run, but each in an isolated
subshell environment without any access to piped standard input and
output streams.



####<a name="Background_Jobs">Background Jobs</a>##




The single ampersand (`&`) conjunction marks the preceding command
group as a background job as per `bash` (Bourne Again Shell) syntax.



####<a name="Quoting_Contexts">Quoting Contexts</a>##




Single quoted (`'...'`), double quoted (`"..."`), and back quoted
(``...``) character sequences are supported, as per the
Bourne shell, as is use of the backslash (`\`) to escape special
characters.  Additionally, single and double quoted sequences have
special meaning in [Erlang Context](#Erlang_Context) and when passed as
Erlang command parameters.  (Bourne-standard quote removal does not
occur in Erlang Context or for Erlang command parameters.)



Single quotes remove the special meaning of all characters they enclose.
In addition, single quotes mark the `atom()` type in Erlang Context and
when passed to Erlang functions.



Double quotes remove the special meaning of all characters other than
the dollar sign (`$`), the back quote (```), the
backslash (`\`), and the newline (per [Multiline Parsing](#Multiline_Parsing)).
Additionally, double quotes indicate the `string()` type, _i.e._
a `list()` of characters, in Erlang Context and when passed as Erlang
command parameters.



Strings are passed to Erlang functions only after all embedded
[Substitution Contexts](#Substitution_Contexts) have been evaluated.



Back quotes mark [Command Substitution](#Command_Substitution), the back quoted
character sequence being replaced by the results of the substituted
command(s) execution.



The [Lists](#Lists) back quote (`[`...`]`) and
[Tuples](#Tuples) back quote (`{`...`}`) are two special
constructs for passing command substitution results as
Erlang-compatible [Term Contexts](#Term_Contexts) rathern than
Bourne-standard whitespace delimited [Words](#Words).



The backslash character (`\`) operates to escape the following
character, as per both Bourne and Erlang syntax.



####<a name="Multiline_Parsing">Multiline Parsing</a>##




Presently, multi-line parsing is not supported.  Any line ending in a
backslash (an escaped carriage return) will result in an error.



###<a name="Substitution_Contexts">Substitution Contexts</a>##




####<a name="Parameter_Expansion">Parameter Expansion</a>##




_Not yet implemented._



####<a name="Command_Substitution">Command Substitution</a>##




_Not yet implemented_.



####<a name="Arithmetic_Expansion">Arithmetic Expansion</a>##




_Not yet implemented_.



###<a name="Term_Contexts">Term Contexts</a>##




####<a name="Words">Words</a>##




_Not yet implemented_.



The simplest `nosh` command line consists of a command followed by zero
or more word contexts, each being delimited by the characters in the
`IFS` environment variable (or `<space>`, `<tab>` and `<newline>`
if `IFS` is undefined).  Each single and/or double quoted context is
treated as a word for this purpose (see [Quoting Contexts](#Quoting_Contexts)).



Unquoted (_i.e._ bare) words that begin with a lower case
character and single quote contexts are passed to Erlang functions as
type `atom()`, while double quote contexts are passed as type `string()`.



Bare words that begin with an upper case character are treated as
Erlang single-assignment variables as any such variables are defined
in the current shell.  (See also [Parameter Expansion](#Parameter_Expansion))



####<a name="Lists">Lists</a>##




_Not yet implemented_.



Zero or more Term contexts are marked as within a single List context
(not to be confused with a [Parameter List](#Parameter_List)) when enclosed by
an open square bracket (`[`) and close square bracket (`]`).  Per
Erlang syntax, elements of a List are delimited by commas (`,`),
whitespace is ignored, and `IFS` specified delimiters have no special
meaning.



[Command Substitution](#Command_Substitution) may be used to generate an Erlang List,
through use of the Lists back quote construct (`[`...`]`).


<h5><a name="Function_Parameter_List">Function Parameter List</a></h5>




_Not yet implemented_.



A Function Parameter List follows Erlang syntax for specifying the
parameters (and indirectly, the arity) of a function, and consists of
zero or more comma-delimited Term contexts enclosed by an
open parentheses (`(`) and close parentheses (`)`) rather than square
brackets.  A Parameter List may only appear in second position after a
command, and only in explicit function mode (see [`nosh_exec`](nosh_exec.md).



In first position, parenthese denote a [Grouping Context](#Grouping_Context).



####<a name="Tuples">Tuples</a>##




_Not yet implemented_.



One or more Term contexts are marked as within a single Tuple
context when enclosed by an open curly bracket (`{`) and close curly
bracket (`}`).  Per Erlang syntax, elements of a Tuple are delimited by
commas (`,`), whitespace is ignored, and `$IFS` specified delimiters
have no special meaning.



[Command Substitution](#Command_Substitution) may be used to generate an Erlang Tuple,
through use of the Tuples back quote construct (`{`...`}`).



####<a name="Pids">Pids</a>##




_Not yet implemented_.



####<a name="Bitstrings">Bitstrings</a>##


_Not yet implemented_.
<a name="types"></a>

##Data Types##




###<a name="type-context">context()</a>##



	context() = nonempty_string() | {<a href="#type-context_desc">context_desc()</a>, <a href="#type-context_list">context_list()</a>}



###<a name="type-context_desc">context_desc()</a>##



	context_desc() = {context, <a href="#type-context_type">context_type()</a>}



###<a name="type-context_list">context_list()</a>##



	context_list() = [<a href="#type-context">context()</a>]



###<a name="type-context_type">context_type()</a>##



	context_type() = <a href="#type-term_type">term_type()</a> | <a href="#type-quote_type">quote_type()</a> | <a href="#type-group_type">group_type()</a> | <a href="#type-exec_type">exec_type()</a>



###<a name="type-exec_type">exec_type()</a>##



	exec_type() = brne | erln



###<a name="type-group_type">group_type()</a>##



	group_type() = pren | semi | ifok | ambi | ifnz | pipe



###<a name="type-parse_error">parse_error()</a>##



	parse_error() = {<a href="#type-parse_error_type">parse_error_type()</a>, string()}



###<a name="type-parse_error_type">parse_error_type()</a>##



	parse_error_type() = quote | group



###<a name="type-quote_type">quote_type()</a>##



	quote_type() = line | back | doub | sing | escp | dbcp



###<a name="type-term_type">term_type()</a>##



	term_type() = word | list | plst | tupl | epid | bstr
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>Parse command line string and return a list of nested contexts,
or else <code>failed</code> on a caught syntax exception.</td></tr><tr><td valign="top"><a href="#parse-3">parse/3</a></td><td>Parse list of strings split on quoting and grouping characters,
according to current Stack type.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="parse-2"></a>

###parse/2##


	parse(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Subject::nonempty_string()) -> {ok, <a href="#type-context_list">context_list()</a>} | {error, <a href="#type-parse_error">parse_error()</a>}
<br></br>




Parse command line string and return a list of nested contexts,
or else `failed` on a caught syntax exception.

Handle thrown errors for unmatched quoting, grouping, and term context
symbols.<a name="parse-3"></a>

###parse/3##


`parse(Type, Stack, List) -> any()`



Parse list of strings split on quoting and grouping characters,
according to current Stack type.  Return tuple of context tree, context
stack and unparsed tail OR tuple of 'close_context', context stack, and
trailing context tree.

Throw exception for unmatched quoting or grouping character.

__<font color="red">To do</font>__
<br></br>
* <font color="red">spec this function</font>
