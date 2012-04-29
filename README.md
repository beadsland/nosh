

#Welcome to nosh#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

###<a name="Introduction">Introduction</a>##


 

`nosh` is a [Bourne Shell Emulator](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Shell_Emulator), [Erl Shell Alternative](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erl_Shell_Alternative)
 and [NoSQL File System](http://github.com/beadsland/nosh/blob/master/doc/README.md#NoSQL_File_System) implemented in Erlang.  Providing an intuitive
 commandline interface for compiling and loading Erlang modules, and for executing
 Erlang module functions, `nosh` affords both the ease of a UNIX shell and the power 
of an Erlang interpreter.

 

_<strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't do much yet._

 

###<a name="Contents">Contents</a>##


 
 * [Noterm](http://github.com/beadsland/nosh/blob/master/doc/README.md#Noterm)

 * [Bourne Shell Emulator](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Shell_Emulator)

 * [Erl Shell Alternative](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erl_Shell_Alternative)

 * [NoSQL File System](http://github.com/beadsland/nosh/blob/master/doc/README.md#NoSQL_File_System)

 

 

###<a name="Noterm">Noterm</a>##


 

A `nosh` process is launched by first starting `noterm`, which handles keyboard input, sending
 this to the `nosh` process as messages, and presenting output and errors to the user as they
 are received in message form from the `nosh` process.

 

_Full terminal emulation has yet to be implemented._  The `nosh` process continues to
 run until it receives an end-of-file message from `noterm`.  This is currently produced by
 typing a period (`.`) by itself on a line, followed by a `<newline>`.  (This EOF period should
 not be confused with the [Erlang Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erlang_Context) left-wise conjunction; or with the period
 command, one of the built-in [Bourne Commands](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Commands).)

 

###<a name="Bourne_Shell_Emulator">Bourne Shell Emulator</a>##


 

`nosh` emulates much of the functionality of the Bourne shell (`sh`), including [Quoting](http://github.com/beadsland/nosh/blob/master/doc/README.md#Quoting) and [Grouping](http://github.com/beadsland/nosh/blob/master/doc/README.md#Grouping).

 

Currently all `nosh` does is parse a command line and return a tree representing quoting and grouping
 relationships.  _Execution functions are not yet implemented_.

 

####<a name="Bourne_Context">Bourne Context</a>##

	
 

Two syntax regimes are supported by `nosh`, identified as Bourne Context and [Erlang Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erlang_Context).
 Bourne Context is the emulation of `sh` syntax, marked by [Grouping](http://github.com/beadsland/nosh/blob/master/doc/README.md#Grouping) rules and parameters 
written as arrays of space-delimited tokens.

 

For a discussion of how Erlang modules and functions are used in the Bourne Context, see the
 [Erl Shell Alternative](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erl_Shell_Alternative) section.

 

####<a name="Quoting">Quoting</a>##


 

Single quoted (`'...'`), double quoted (`"..."`), and back quoted (``...``) character
 sequences are supported, as per the Bourne shell, as is use of the backslash (`\`) to escape special
 characters.  Additionally, single and double quoted sequences have special meaning
 when passed to Erlang functions.  (Unlike in Bourne shell, quote removal does not occur in `nosh`.)

 

Single quotes remove the special meaning of all characters they enclose.  In addition, single quotes
 indicate the `atom()` type when passed to Erlang functions.

 

Double quotes remove the special meaning of all characters other than the dollar sign (`$`), the
 back quote (```), the backslash (`\`), and the newline (per [Multiline Parsing](http://github.com/beadsland/nosh/blob/master/doc/README.md#Multiline_Parsing)).
 Additionally, double quotes indicate the `string()` type, _i.e._ a `list()` of characters.
 Strings are passed to Erlang functions only after all embedded [Parameter Expansion](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameter_Expansion),
 [Command Substition](http://github.com/beadsland/nosh/blob/master/doc/README.md#Command_Substition) and [Arithmetic Expansion](http://github.com/beadsland/nosh/blob/master/doc/README.md#Arithmetic_Expansion) has been performed.

 

Back quotes signal [Command Substitution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Command_Substitution), the back quoted character sequence being replaced
 by the results of the substituted command(s) execution.  A [Lists](http://github.com/beadsland/nosh/blob/master/doc/README.md#Lists) back quote (`[`...`]`)
 results in an Erlang `list()` of tokens.  Otherwise, the results of a back quoted sequence are inserted 
into the Bourne-standard token array as space-delimited parameters.

 

Escaped characters work as expected.

 
<h5><a name="Multiline_Parsing">Multiline Parsing</a></h5>


 

Presently, multi-line parsing is not supported.  Any line ending in a backslash (an escaped carriage return) 
will result in an error.

 

####<a name="Grouping">Grouping</a>##


 

Grouping is parsed for entire line prior to execution, unlike Bourne standard. 
This enforces transactional integrity:  command execution only occurs if entire command sequence parses correctly.

 

In first position, parentheses (`(...)`) group commands within for execution in a [Subshell Environment](http://github.com/beadsland/nosh/blob/master/doc/README.md#Subshell_Environment),
 as per [Command Substitution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Command_Substitution).  A parentheses group appearing in second position, following
 a command, is treated as an [Parameter List](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameter_List).  Parentheses groups
 after second position are invalid, throwing a parsing error.  (_Second and third position processing not yet implemented._)

 

The faux within-shell execution syntax of Bourne shell (`{ ...; };`)--implemented
 with the open curly bracket (`{`) and closed curly bracker (`}`) as reserved words--is not
 supported by `nosh`, curly brackets instead marking [Tuples](http://github.com/beadsland/nosh/blob/master/doc/README.md#Tuples).

 

The semicolon (`;`) conjunction works in `nosh` as it does in Bourne shell, execution of the command group 
following the semicolon being deferred until the command group preceding the semicolon has completed.

 

Likewise, double ampersand (`&&`) and double vertical bar (`||`) afford conditional execution.  The command
 group following a double ampersand executing only on a zero status (an Erlang return value of `ok` or `{ok, ...}`), 
and the command group following a double vertical bar executing only on a non-zero status (any other Erlang return value).

 

These [Bourne Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Context) conjunctions are parsed right-wise.  That is, everything that follows the conjunction
 is returned as a nested group in the last element of the enclosing group context.  There are two left-wise conjunctions,
 the period (`.`), indicating an [Erlang Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erlang_Context) group, and the `<newline>`, 
indicating the enclosing command line group.

 
<h5><a name="Pipes">Pipes</a></h5>


 

As per Bourne shell, pipes are marked by a single vertical bar (`|`).  Erlang processes may participate in pipe
 relationships if they implement the [Nosh_exec Behaviour](http://github.com/beadsland/nosh/blob/master/doc/README.md#Nosh_exec_Behaviour).  Otherwise, processes grouped by the single vertical bar
 will run, but each in an isolated [Subshell Environment](http://github.com/beadsland/nosh/blob/master/doc/README.md#Subshell_Environment) without any access to piped standard input and output streams.

 
<h5><a name="Background_Jobs">Background Jobs</a></h5>


 

The single ampersand (`&`) marks a background job as per `bash` (Bourne Again Shell) syntax.
 _The `jobs` command and `%` syntax for bringing jobs to the foreground have not yet been implemented._

 

####<a name="Parameter_Expansion">Parameter Expansion</a>##


 

_Not yet implemented._

 

####<a name="Command_Substitution">Command Substitution</a>##


 

_Not yet implemented_.

 

####<a name="Arithmetic_Expansion">Arithmetic Expansion</a>##


 

_Not yet implemented_.

 

####<a name="Subshell_Environment">Subshell Environment</a>##


 

_Command execution, including shell and subshell environments, has not yet been implemented._

 

####<a name="Bourne_Commands">Bourne Commands</a>##


 

_Bourne built-in commands have not yet been implemented._

 

###<a name="Erl_Shell_Alternative">Erl Shell Alternative</a>##


 

`nosh` will provide a UNIX-style command line for Erlang, as a human-thinkable alternative to
 the Erl shell ([`http://www.erlang.org/doc/man/erl.html`](http://www.erlang.org/doc/man/erl.html)).

 

The Erlang syntax, while perfectly appropriate for functional programming, can prove frustrating in the 
procedural context of a shell environment.  Further, exception handling in the Erl shell is less than 
graceful, and compiling and loading modules can be unduly cumbersome and non-intuitive.

 

For these reasons, `nosh` operates in [Bourne Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Context) by default, while still supporting
 Erlang [Lists](http://github.com/beadsland/nosh/blob/master/doc/README.md#Lists) and [Tuples](http://github.com/beadsland/nosh/blob/master/doc/README.md#Tuples).  Within Bourne Context, [Modules](http://github.com/beadsland/nosh/blob/master/doc/README.md#Modules) compile
 and load on-the-fly, [Parameters](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameters) may be passed (and functions called) with minimal typing,
 and [Exceptions](http://github.com/beadsland/nosh/blob/master/doc/README.md#Exceptions) thrown by processes are gracefully caught and displayed.

 

####<a name="Modules">Modules</a>##


 

_None of this has yet been implemented._

 

Each Erlang module is treated as an executable in `nosh`.  When the name of a module appears
 in first position on a `nosh` command line, a matching `.beam` file is sought on each directory
 on the `$PATH` environment variable, with one modification.  For each directory on `$PATH` that
 ends in `ebin\`, and for which the current user has write access, the shell will look for a
 parallel `src\` directory, and if found, search for a matching `.erl` file therein.

 

If an associated `.erl` file is found, and it is newer that the `.beam` file, or if an `.erl` file
 is found for which no `.beam` file appears, the `.erl` file will be compiled to its `ebin\`
 directory.  If this compilation is successful, the module will be loaded and [Execution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Execution) proceeds.
 Otherwise, the compiler error is written to `stdout` and a non-zero status is returned.

 

If no associated `.erl` file is found, the `.beam` file on the $PATH is loaded and [Execution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Execution)
 goes forward.  If no `.beam` file is found, the search continues to the next directory on `$PATH`,
 returning an error if no `.beam` file can be found or compiled from source before the `$PATH` is 
exhausted.

 
<h5><a name="Execution">Execution</a></h5>


 

Module execution may be invoked in one of three modes:  implicit start, explicit function or 
getoptions functions.

 

In _implicit start_ mode, the module name appears in first position (as a command) with or without parameters.
 If without parameters, `nosh` will attempt to spawn a process running the module's `start/4` function,
 or else `start/3`, as specified by the [Nosh_exec Behaviour](http://github.com/beadsland/nosh/blob/master/doc/README.md#Nosh_exec_Behaviour), and failing that,
 will attempt to spawn on `start/0`, reporting an error on failure.

 

If with parameters, `nosh` will first call any getoptions functions, caching the results from each.
 If any getoptions functions were found, `nosh` will then attempt to spawn a process running the
 module's [Nosh_exec Behaviour](http://github.com/beadsland/nosh/blob/master/doc/README.md#Nosh_exec_Behaviour) `start/6` function, reporting an error on failure.  Otherwise,
 `nosh` will attempt to spawn on `start/5`, reporting an error on failure.

 

In _explicit function_ mode, the command appears in Erlang syntax as _`Module`_`:`_`Function`_.
 This can stand on it's own, or else be followed either by [Parameters](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameters) or a [Parameter List](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameter_List).
 The resulting arity function is called by `nosh`, reporting an error on failure.  In [Erlang Context](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erlang_Context), 
all function invocations are in explicit function mode, always with a Parameter List.

 

In _getoptions functions_ mode, the module name appears in first position, followed by
 [Parameters](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameters), including one or more 0-arity and/or 1-arity function names specified in Perl getoptions
 ([`http://aplawrence.com/Unix/perlgetopts.html`](http://aplawrence.com/Unix/perlgetopts.md)) syntax.  These functions are executed in order, before
 `nosh` switches to implicit start mode, as described above.

 
<h5><a name="Subshell_Environment">Subshell Environment</a></h5>


 

_Not yet implemented._

 
<h5><a name="Nosh_exec_Behaviour">Nosh_exec Behaviour</a></h5>


 

_Not yet implemented._

 

Canonical spawn entry function:  `start(stdin, stdout, stderr, env, arg, getoptions)`

 

####<a name="Parameters">Parameters</a>##


 

Parameters may be Bourne-style [Words](http://github.com/beadsland/nosh/blob/master/doc/README.md#Words), including character sequences delimited by single or double
 [Quoting](http://github.com/beadsland/nosh/blob/master/doc/README.md#Quoting).  In addition, Erlang-syntax [Lists](http://github.com/beadsland/nosh/blob/master/doc/README.md#Lists) and/or [Tuples](http://github.com/beadsland/nosh/blob/master/doc/README.md#Tuples) may be
 specified as parameters.  [Parameter List](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameter_List) syntax is a special case only valid in
 explicit function [Execution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Execution) mode.

 
<h5><a name="Words">Words</a></h5>


 

The simplest `nosh` command line consists of a command followed by zero or more words, this
 sequence being delimited by the characters in the `$IFS` environment variable, or `<space>`, `<tab>` and `<newline>`
 if `$IFS` is undefined.  Each single and/or double quoted sequence is treated as a word for this purpose
 (see [Quoting](http://github.com/beadsland/nosh/blob/master/doc/README.md#Quoting)).

 

Unquoted (_i.e._ bare) words and single quoted words are passed to Erlang functions as type `atom()`, while
 double quoted words are passed as type `string()`.

 
<h5><a name="Lists">Lists</a></h5>


 

Zero or more Words, Lists and/or Tuples are treated as a single List parameter
 (not to be confused with a [Parameter List](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameter_List)) when enclosed by an open square bracket (`[`)
 and close square bracket (`]`).  Per Erlang syntax, elements of a List are delimited by commas (`,`),
 whitespace is ignored, and `$IFS` specified delimiters have no special meaning.

 

[Command Substitution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Command_Substitution) may be used to generate an Erlang List, by nesting back quotes
 within square brackets (`[`...`]`).

 
<h5><a name="Parameter_List">Parameter List</a></h5>


 

A Parameter List follows Erlang syntax for specifying the parameters (and indirectly, the arity) of a
 function, and consists of zero or more comma-delimited Words, Lists, and Tuples enclosed by an
 open parentheses (`(`) and close parentheses (`)`) rather than square brackets.  A Parameter List
 may only appear in second position after a command, and only in explicit function mode (see [Execution](http://github.com/beadsland/nosh/blob/master/doc/README.md#Execution)).

 

In first position, parentheses denote command [Grouping](http://github.com/beadsland/nosh/blob/master/doc/README.md#Grouping).

 
<h5><a name="Tuples">Tuples</a></h5>


 

One or more Words, Lists and/or Tuples are treated as a single Tuple parameter when
 enclosed by an open curly bracket (`{`) and close curly bracket (`}`).  Per Erlang syntax,
 elements of a Tuple are delimited by commas (`,`), whitespace is ignored, and `$IFS` specified 
delimiters have no special meaning.

 

####<a name="Exceptions">Exceptions</a>##


 

The `nosh` process and/or [Subshell Environment](http://github.com/beadsland/nosh/blob/master/doc/README.md#Subshell_Environment) that calls or spawns any given module function
 catches all exceptions thrown thereby, including those that result in a non-`normal` status process `'EXIT`''.
 By default, any such exception is printed to `stderr`, followed by a stack trace, and a non-zero exit
 status is passed to any conditional [Grouping](http://github.com/beadsland/nosh/blob/master/doc/README.md#Grouping).

 

####<a name="Erl_Commands">Erl Commands</a>##


 

_Not yet implemented:_  All functions available through the Erl shell are available as `nosh` commands.

 

####<a name="Erlang_Context">Erlang Context</a>##


 

_Not yet implemented:_  The Erlang Context is a special case of [Grouping](http://github.com/beadsland/nosh/blob/master/doc/README.md#Grouping), denoted by the left-wise
 conjunction, period (`.`).  When the `nosh` command parser encounters a period on it's own (not as part of any
 [Parameters](http://github.com/beadsland/nosh/blob/master/doc/README.md#Parameters)), it rolls everything in the current context to the left of the period into a sub-context, 
and then continues parsing the sequence that follows within the current context.

 

Any semicolon to be included in Erlang Context must be escaped (`\;`).  Otherwise, the semicolon will be interpreted 
as a grouping conjunction delimiting the start of a context, rather than a syntantical marker within the current 
Erlang Context.

 

The period conjunction should not be confused with either the [Noterm](http://github.com/beadsland/nosh/blob/master/doc/README.md#Noterm) end-of-file character or the period
 command, one of the built-in [Bourne Commands](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Commands).

 

###<a name="NoSQL_File_System">NoSQL File System</a>##


 _Not yet implemented:_  `nosh` will provide an NFS-style interface to CouchDB databases and views.


##Packages##


<table width="100%" border="0" summary="list of packages"><tr><td><a href="http://github.com/beadsland/nosh/blob/master/alt/package-summary.md" class="package">alt</a></td></tr><tr><td><a href="http://github.com/beadsland/nosh/blob/master/alt2/package-summary.md" class="package">alt2</a></td></tr></table>



##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh.md" class="module">nosh</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_eval.md" class="module">nosh_eval</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_exec.md" class="module">nosh_exec</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_load.md" class="module">nosh_load</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md" class="module">nosh_parse</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/noterm.md" class="module">noterm</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/test.md" class="module">test</a></td></tr></table>

