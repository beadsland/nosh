

#Welcome to nosh#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">confirm still work w/o dev
</font>
* <font color="red">erl_opts deps w/out ../..
</font>
* <font color="red">escript folderl
</font>
* <font color="red">-*-Erlang-*- in license line 1
</font>
* <font color="red">escript nosh
</font>
* <font color="red">refactor gen_command:load_command pattern
</font>
* <font color="red">noterm - refactor console as gen_command
</font>
* <font color="red">...plus 48 more (see TODO.edoc)
</font>


`nosh` is a Bourne shell emulator, Erl shell alternative, and
  noSQL file system,  implemented in Erlang.  Providing an intuitive
  commandline interface for compiling and loading Erlang modules, and for
  executing Erlang module functions, `nosh` affords both the ease of a
UNIX shell and the power of an Erlang interpreter.
 


_<strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't  do much yet._
 

* [Installation](#Installation)

* [Bourne Shell Emulator](#Bourne_Shell_Emulator)

* [Erl Shell Alternative](#Erl_Shell_Alternative)

* [NoSQL File System](#NoSQL_File_System)


 


###<a name="Installation">Installation</a>##

 


Presently, `nosh` runs as a Makefile target.  The simplest way to get
  started with `nosh` is to change to the nosh project directory in a
  terminal session and type `make run`.
 


Users of the Eclipse IDE may integrate `nosh` shell with their
  workflow by installing the Wicked Shell plugin
[`http://www.wickedshell.net/`](http://www.wickedshell.net/) from the Eclipse Marketplace.  Simply
  run `bash` or `sh` under Wicked Shell, and then execute `make run`
from there.
 


###<a name="Bourne_Shell_Emulator">Bourne Shell Emulator</a>##

 


`nosh` emulates much of the functionality of the Bourne shell (`sh`).
 


Currently all `nosh` does is [parse](nosh_parse.md) a command line
  and return a tree representing quoting and grouping relationships.
_Execution functions are not yet implemented_.
 


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
[`http://www.erlang.org/doc/man/erl.html`](http://www.erlang.org/doc/man/erl.html).
 


The Erlang syntax, while perfectly appropriate for functional
programming, can prove frustrating in the imperative paradigm of a
shell environment, and compiling and loading modules can be unduly
cumbersome and non-intuitive.
 


`nosh` operates in Bourne Context by default, while still supporting
  Erlang data types, and allowing Erlang Context to be embedded in
  commandlines as needed.  Within Bourne Context, Erlang modules are
  invoked like UNIX commands, and [compile and load
  on-the-fly](nosh_load.md), parameters being passed (and functions called) with
minimal typing.
 


####<a name="Erl_Commands">Erl Commands</a>##

 


_Not yet implemented:_  All functions available through the Erl
  shell are available as `nosh` commands.
 


####<a name="Erlang_Context">Erlang Context</a>##

 


_Not yet implemented:_  Erlang style syntax can be used on the
`nosh` command line as per the Erl shell.  See [`nosh_parse`](nosh_parse.md) for
more details.
 


###<a name="NoSQL_File_System">NoSQL File System</a>##

 
_Not yet implemented:_`nosh` will provide an NFS-style interface
  to CouchDB databases and views.
 

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="nosh.md" class="module">nosh</a></td></tr>
<tr><td><a href="nosh_context.md" class="module">nosh_context</a></td></tr>
<tr><td><a href="nosh_eval.md" class="module">nosh_eval</a></td></tr>
<tr><td><a href="nosh_exec.md" class="module">nosh_exec</a></td></tr>
<tr><td><a href="nosh_parse.md" class="module">nosh_parse</a></td></tr></table>

