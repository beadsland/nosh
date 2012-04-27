[Overview](overview-summary.html)

[![erlang logo](erlang.png)](http://www.erlang.org/)

# Welcome to nosh

Copyright © 2012 Beads D. Land-Trujillo

**Version:** 0.1.0 

**Authors:** Beads D. Land-Trujillo [_web site:_ `[http://twitter.com/beadsland](http://twitter.com/beadsland)`].

### Introduction

`nosh` is a Bourne Shell Emulator, Erl Shell Alternative and NoSQL File System
implemented in Erlang. Providing an intuitive commandline interface for
compiling and loading Erlang modules, and for executing Erlang module
functions, `nosh` affords both the ease of a UNIX shell and the power of an
Erlang interpreter.

### Contents

  * Noterm
  * Bourne Shell Emulator
  * Erl Shell Alternative
  * NoSQL File System

### Noterm

A `nosh` process is launched by first starting `noterm`, which handles
keyboard input, sending this to the `nosh` process as messages, and presenting
output and errors to the user as they are received in message form from the
`nosh` process.

_Full terminal emulation has yet to be implemented._ The `nosh` process
continues to run until it receives an end-of-file message from `noterm`. This
is currently produced by typing a period (`.`) by itself on a line, followed
by a `<newline>`. (This EOF period should not be confused with the Erlang
Context left-wise conjunction; or with the period command, one of the built-in
Bourne Commands.)

### Bourne Shell Emulator

`nosh` emulates much of the functionality of the Bourne shell (`sh`),
including Quoting and Grouping.

Currently all `nosh` does is parse a command line and return a tree
representing quoting and grouping relationships. _Execution functions are not
yet implemented_.

#### Bourne Context

Two syntax regimes are supported by `nosh`, identified as Bourne Context and
Erlang Context. Bourne Context is the emulation of `sh` syntax, marked by
Grouping rules and parameters written as arrays of space-delimited tokens.

For a discussion of how Erlang modules and functions are used in the Bourne
Context, see the Erl Shell Alternative section.

#### Quoting

Single quoted (`'...'`), double quoted (`"..."`), and back quoted (``...``)
character sequences are supported, as per the Bourne shell, as is use of the
backslash (`\`) to escape special characters. Additionally, single and double
quoted sequences have special meaning when passed to Erlang functions. (Unlike
in Bourne shell, quote removal does not occur in `nosh`.)

Single quotes remove the special meaning of all characters they enclose. In
addition, single quotes indicate the `atom()` type when passed to Erlang
functions.

Double quotes remove the special meaning of all characters other than the
dollar sign (`$`), the back quote (```), the backslash (`\`), and the newline
(per Multiline Parsing). Additionally, double quotes indicate the `string()`
type, _i.e._ a `list()` of characters. Strings are passed to Erlang functions
only after all embedded Parameter Expansion, Command Substition and Arithmetic
Expansion has been performed.

Back quotes signal Command Substitution, the back quoted character sequence
being replaced by the results of the substituted command(s) execution. A Lists
back quote (`[`...`]`) results in an Erlang `list()` of tokens. Otherwise, the
results of a back quoted sequence are inserted into the Bourne-standard token
array as space-delimited parameters.

Escaped characters work as expected.

##### Multiline Parsing

Presently, multi-line parsing is not supported. Any line ending in a backslash
(an escaped carriage return) will result in an error.

#### Grouping

Grouping is parsed for entire line prior to execution, unlike Bourne standard.
This enforces transactional integrity: command execution only occurs if entire
command sequence parses correctly.

In first position, parentheses (`(...)`) group commands within for execution
in a Subshell Environment, as per Command Substitution. A parentheses group
appearing in second position, following a command, is treated as an Parameter
List. Parentheses groups after second position are invalid, throwing a parsing
error. (_Second and third position processing not yet implemented._)

The faux within-shell execution syntax of Bourne shell (`{ ...;
};`)--implemented with the open curly bracket (`{`) and closed curly bracker
(`}`) as reserved words--is not supported by `nosh`, curly brackets instead
marking Tuples.

The semicolon (`;`) conjunction works in `nosh` as it does in Bourne shell,
execution of the command group following the semicolon being deferred until
the command group preceding the semicolon has completed.

Likewise, double ampersand (`&&`) and double vertical bar (`||`) afford
conditional execution. The command group following a double ampersand
executing only on a zero status (an Erlang return value of `ok` or `{ok,
...}`), and the command group following a double vertical bar executing only
on a non-zero status (any other Erlang return value).

These Bourne Context conjunctions are parsed right-wise. That is, everything
that follows the conjunction is returned as a nested group in the last element
of the enclosing group context. There are two left-wise conjunctions, the
period (`.`), indicating an Erlang Context group, and the `<newline>`,
indicating the enclosing command line group.

##### Pipes

As per Bourne shell, pipes are marked by a single vertical bar (`|`). Erlang
processes may participate in pipe relationships if they implement the
Nosh_exec Behaviour. Otherwise, processes grouped by the single vertical bar
will run, but each in an isolated Subshell Environment without any access to
piped standard input and output streams.

##### Background Jobs

The single ampersand (`&`) marks a background job as per `bash` (Bourne Again
Shell) syntax. _The `jobs` command and `%` syntax for bringing jobs to the
foreground have not yet been implemented._

#### Parameter Expansion

_Not yet implemented._

#### Command Substitution

_Not yet implemented_.

#### Arithmetic Expansion

_Not yet implemented_.

#### Subshell Environment

_Command execution, including shell and subshell environments, has not yet
been implemented._

#### Bourne Commands

_Bourne built-in commands have not yet been implemented._

### Erl Shell Alternative

`nosh` will provide a UNIX-style command line for Erlang, as a human-thinkable
alternative to the Erl shell ([`http://www.erlang.org/doc/man/erl.html`](http:
//www.erlang.org/doc/man/erl.html)).

The Erlang syntax, while perfectly appropriate for functional programming, can
prove frustrating in the procedural context of a shell environment. Further,
exception handling in the Erl shell is less than graceful, and compiling and
loading modules can be unduly cumbersome and non-intuitive.

For these reasons, `nosh` operates in Bourne Context by default, while still
supporting Erlang Lists and Tuples. Within Bourne Context, Modules compile and
load on-the-fly, Parameters may be passed (and functions called) with minimal
typing, and Exceptions thrown by processes are gracefully caught and
displayed.

#### Modules

_None of this has yet been implemented._

Each Erlang module is treated as an executable in `nosh`. When the name of a
module appears in first position on a `nosh` command line, a matching `.beam`
file is sought on each directory on the `$PATH` environment variable, with one
modification. For each directory on `$PATH` that ends in `ebin\`, and for
which the current user has write access, the shell will look for a parallel
`src\` directory, and if found, search for a matching `.erl` file therein.

If an associated `.erl` file is found, and it is newer that the `.beam` file,
or if an `.erl` file is found for which no `.beam` file appears, the `.erl`
file will be compiled to its `ebin\` directory. If this compilation is
successful, the module will be loaded and Execution proceeds. Otherwise, the
compiler error is written to `stdout` and a non-zero status is returned.

If no associated `.erl` file is found, the `.beam` file on the $PATH is loaded
and Execution goes forward. If no `.beam` file is found, the search continues
to the next directory on `$PATH`, returning an error if no `.beam` file can be
found or compiled from source before the `$PATH` is exhausted.

##### Execution

Module execution may be invoked in one of three modes: implicit start,
explicit function or getoptions functions.

In _implicit start_ mode, the module name appears in first position (as a
command) with or without parameters. If without parameters, `nosh` will
attempt to spawn a process running the module's `start/4` function, or else
`start/3`, as specified by the Nosh_exec Behaviour, and failing that, will
attempt to spawn on `start/0`, reporting an error on failure.

If with parameters, `nosh` will first call any getoptions functions, caching
the results from each. If any getoptions functions were found, `nosh` will
then attempt to spawn a process running the module's Nosh_exec Behaviour
`start/6` function, reporting an error on failure. Otherwise, `nosh` will
attempt to spawn on `start/5`, reporting an error on failure.

In _explicit function_ mode, the command appears in Erlang syntax as
_`Module`_`:`_`Function`_. This can stand on it's own, or else be followed
either by Parameters or a Parameter List. The resulting arity function is
called by `nosh`, reporting an error on failure. In Erlang Context, all
function invocations are in explicit function mode, always with a Parameter
List.

In _getoptions functions_ mode, the module name appears in first position,
followed by Parameters, including one or more 0-arity and/or 1-arity function
names specified in Perl getoptions ([`http://aplawrence.com/Unix/perlgetopts.h
tml`](http://aplawrence.com/Unix/perlgetopts.html)) syntax. These functions
are executed in order, before `nosh` switches to implicit start mode, as
described above.

##### Subshell Environment

_Not yet implemented._

##### Nosh_exec Behaviour

_Not yet implemented._

Canonical spawn entry function: `start(stdin, stdout, stderr, env, arg,
getoptions)`

#### Parameters

Parameters may be Bourne-style Words, including character sequences delimited
by single or double Quoting. In addition, Erlang-syntax Lists and/or Tuples
may be specified as parameters. Parameter List syntax is a special case only
valid in explicit function Execution mode.

##### Words

The simplest `nosh` command line consists of a command followed by zero or
more words, this sequence being delimited by the characters in the `$IFS`
environment variable, or `<space>`, `<tab>` and `<newline>` if `$IFS` is
undefined. Each single and/or double quoted sequence is treated as a word for
this purpose (see Quoting).

Unquoted (_i.e._ bare) words and single quoted words are passed to Erlang
functions as type `atom()`, while double quoted words are passed as type
`string()`.

##### Lists

Zero or more Words, Lists and/or Tuples are treated as a single List parameter
(not to be confused with a Parameter List) when enclosed by an open square
bracket (`[`) and close square bracket (`]`). Per Erlang syntax, elements of a
List are delimited by commas (`,`), whitespace is ignored, and `$IFS`
specified delimiters have no special meaning.

Command Substitution may be used to generate an Erlang List, by nesting back
quotes within square brackets (`[`...`]`).

##### Parameter List

A Parameter List follows Erlang syntax for specifying the parameters (and
indirectly, the arity) of a function, and consists of zero or more comma-
delimited Words, Lists, and Tuples enclosed by an open parentheses (`(`) and
close parentheses (`)`) rather than square brackets. A Parameter List may only
appear in second position after a command, and only in explicit function mode
(see Execution).

In first position, parentheses denote command Grouping.

##### Tuples

One or more Words, Lists and/or Tuples are treated as a single Tuple parameter
when enclosed by an open curly bracket (`{`) and close curly bracket (`}`).
Per Erlang syntax, elements of a Tuple are delimited by commas (`,`),
whitespace is ignored, and `$IFS` specified delimiters have no special
meaning.

#### Exceptions

The `nosh` process and/or Subshell Environment that calls or spawns any given
module function catches all exceptions thrown thereby, including those that
result in a non-`normal` status process `'EXIT`''. By default, any such
exception is printed to `stderr`, followed by a stack trace, and a non-zero
exit status is passed to any conditional Grouping.

#### Erl Commands

_Not yet implemented:_ All functions available through the Erl shell are
available as `nosh` commands.

#### Erlang Context

_Not yet implemented:_ The Erlang Context is a special case of Grouping,
denoted by the left-wise conjunction, period (`.`). When the `nosh` command
parser encounters a period on it's own (not as part of any Parameters), it
rolls everything in the current context to the left of the period into a sub-
context, and then continues parsing the sequence that follows within the
current context.

Any semicolon to be included in Erlang Context must be escaped (`\;`).
Otherwise, the semicolon will be interpreted as a grouping conjunction
delimiting the start of a context, rather than a syntantical marker within the
current Erlang Context.

The period conjunction should not be confused with either the Noterm end-of-
file character or the period command, one of the built-in Bourne Commands.

### NoSQL File System

_Not yet implemented:_ `nosh` will provide an NFS-style interface to CouchDB
databases and views.

* * *

[Overview](overview-summary.html)

[![erlang logo](erlang.png)](http://www.erlang.org/)

_Generated by EDoc, Apr 27 2012, 04:51:37._

