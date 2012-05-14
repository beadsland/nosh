

#Welcome to nosh#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>

* <font color="red">refactor start to pose.erl
</font>
* <font color="red">pose:exec_run/3
</font>
* <font color="red">pose_spawn_run/3
</font>
* <font color="red">pose ?EXEC_RUN
</font>
* <font color="red">pose ?SPAWN_RUN
</font>
* <font color="red">pose doc
</font>
* <font color="red">pose good
</font>
* <font color="red">flat warn every time
</font>
* <font color="red">pose ensure_submodules
</font>
* <font color="red">pose_code:load_command
</font>
* <font color="red">pose_code:load_project
</font>
* <font color="red">superl short module name
</font>
* <font color="red">todo shortlist
</font>
* <font color="red">packaged import checker
</font>
* <font color="red">single-entry versions?
</font>
* <font color="red">standard pose_cmd behaviour
</font>
* <font color="red">revise nosh docs re nosh_exec behaviour
</font>
* <font color="red">recruitment interviews
</font>
* <font color="red">hot as second subproject load
</font>
* <font color="red">nosh_erl doc
</font>
* <font color="red">refactor bang
</font>
* <font color="red">bang stderr via temp file
</font>
* <font color="red">github standard .gitignore (nosh_nosql)
</font>
* <font color="red">integrate nosql project
</font>
* <font color="red">pose loop behaviour
</font>
* <font color="red">refactor noterm to own project
</font>
* <font color="red">make noterm pose-compliant
</font>
* <font color="red">pose integration with error_logger
</font>
* <font color="red">does term pass params to shell?
</font>
* <font color="red">refactor bang to nosh_erl project
</font>
* <font color="red">bang stderr pipe
</font>
* <font color="red">domain consolidation
</font>
* <font color="red">nosh_parse tokens
</font>
* <font color="red">nosh_eval
</font>
* <font color="red">nosh_exec
</font>
* <font color="red">echo command
</font>
* <font color="red">nosh_erl project
</font>
* <font color="red">refactor hot command
</font>
* <font color="red">nosh_erl module
</font>
* <font color="red">$PATH search
</font>
* <font color="red">doc update:  first character, lower case:  atom; upper case:  variable
</font>
* <font color="red">nosh_parse: tokenizing
</font>
* <font color="red">implement nosh_exec
</font>
* <font color="red">implement nosh_eval to convert nosh_parse output to nosh_exec input
</font>
* <font color="red">finish up nosh_parse
</font>
* <font color="red">nosh as application
</font>
* <font color="red">standardize nosh_exec behaviour
</font>
* <font color="red">nosh as nosh_command
</font>
* <font color="red">Buffered I/O (pending get_char working in noterm)
</font>
* <font color="red">File System:  pwd / cd
</font>
* <font color="red">File System:  *NIX / cygwin / Win32
</font>
* <font color="red">File System:  NoSQL
</font>
* <font color="red">Env
</font>
* <font color="red">Sh commands
</font>
* <font color="red">Erl commands
</font>
* <font color="red">Couch commands
</font>
* <font color="red">Add support for line continuation (currently throws error)
</font>
* <font color="red">integrate with wicked shell</font>


###<a name="Introduction">Introduction</a>##


`nosh` is a Bourne shell emulator, Erl shell alternative, and noSQL
  file system,  implemented in Erlang.  Providing an intuitive
  commandline interface for compiling and loading Erlang modules, and for
  executing Erlang module functions, `nosh` affords both the ease of a
UNIX shell and the power of an Erlang interpreter.

_<strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't  do much yet._
* [Installation](http://github.com/beadsland/nosh/blob/master/doc/README.md#Installation)
* [Bourne Shell Emulator](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Shell_Emulator)
* [Erl Shell Alternative](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erl_Shell_Alternative)
* [NoSQL File System](http://github.com/beadsland/nosh/blob/master/doc/README.md#NoSQL_File_System)


###<a name="Installation">Installation</a>##


Presently, `nosh` runs as a Makefile target.  The simplest way to get
  started with `nosh` is to change to the nosh project directory in a
  terminal session and type `make run`.

Users of the Eclipse IDE may integrate `nosh` shell with their
  workflow by installing the Wicked Shell plugin[`http://www.wickedshell.net/`](http://www.wickedshell.net/) from the Eclipse Marketplace.  Simply
  run `bash` or `sh` under Wicked Shell, and then execute `make run`
from there.

###<a name="Bourne_Shell_Emulator">Bourne Shell Emulator</a>##


`nosh` emulates much of the functionality of the Bourne shell (`sh`).

Currently all `nosh` does is [parse](http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md) a command line
  and return a tree representing quoting and grouping relationships._Execution functions are not yet implemented_.

Two syntax regimes are supported by `nosh`, identified as Bourne Context
  and Erlang Context.  Bourne Context is the emulation of `sh` syntax,
marked by grouping rules and command parameters written as arrays of
space-delimited tokens.

A full suite of core Bourne Shell commands are being implemented as part
  of the __[nosh_bin](http://github.com/beadsland/nosh_bin)__
project.

###<a name="Erl_Shell_Alternative">Erl Shell Alternative</a>##


`nosh` will provide a UNIX-style command line for Erlang, as a
  human-thinkable alternative to the Erl shell
  ([`http://www.erlang.org/doc/man/erl.html`](http://www.erlang.org/doc/man/erl.html)).

The Erlang syntax, while perfectly appropriate for functional
programming, can prove frustrating in the imperative paradigm of a
shell environment, and compiling and loading modules can be unduly
cumbersome and non-intuitive.

`nosh` operates in Bourne Context by default, while still supporting
  Erlang data types, and allowing Erlang Context to be embedded in
  commandlines as needed.  Within Bourne Context, Erlang modules are
  invoked like UNIX commands, and [compile and load
  on-the-fly](http://github.com/beadsland/nosh/blob/master/doc/nosh_load.md), parameters being passed (and functions called) with
minimal typing.

####<a name="Erl_Commands">Erl Commands</a>##


_Not yet implemented:_  All functions available through the Erl
  shell are available as `nosh` commands.

####<a name="Erlang_Context">Erlang Context</a>##


_Not yet implemented:_  Erlang style syntax can be used on the`nosh` command line as per the Erl shell.  See {link nosh_parse} for
more details.

###<a name="NoSQL_File_System">NoSQL File System</a>##
_Not yet implemented:_`nosh` will provide an NFS-style interface
  to CouchDB databases and views.

##Packages##


<table width="100%" border="0" summary="list of packages"><tr><td><a href="http://github.com/beadsland/nosh/blob/master/alt/package-summary.md" class="package">alt</a></td></tr><tr><td><a href="http://github.com/beadsland/nosh/blob/master/alt2/package-summary.md" class="package">alt2</a></td></tr></table>



##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh.md" class="module">nosh</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_bang.md" class="module">nosh_bang</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_context.md" class="module">nosh_context</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_eval.md" class="module">nosh_eval</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_exec.md" class="module">nosh_exec</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md" class="module">nosh_parse</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_test.md" class="module">nosh_test</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/noterm.md" class="module">noterm</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/test.md" class="module">test</a></td></tr></table>

