

#Welcome to nosh#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">!! once 2do_go4 is operating, these will be refactored !!
</font>
* <font color="red">touch base re travel plans		(pending client avail)
</font>
* <font color="red">confirm cat food / litter order		(pending travel plans)
</font>
* <font color="red">onsite meeting with client		(pending client avail)
</font>
* <font color="red">audio tapes				(7 1/2 of 12 cassettes done)	
</font>
* <font color="red">smartware backups laptop		(c: 16,500 of 17,300)
</font>
* <font color="red">macrium disk image laptop 		(pending smartware)
</font>
* <font color="red">...plus 196 more (see TODO.edoc)
</font>


`nosh` is a Bourne shell emulator, Erl shell alternative, and
  noSQL file system,  implemented in Erlang.  Providing an intuitive
  commandline interface for compiling and loading Erlang modules, and for
  executing Erlang module functions, `nosh` affords both the ease of a
UNIX shell and the power of an Erlang interpreter.
 


_<strong>NOTICE:</strong> Code is still in rudimentary form. Doesn't  do much yet._
 

* [Installation](http://github.com/beadsland/nosh/blob/master/doc/README.md#Installation)

* [Bourne Shell Emulator](http://github.com/beadsland/nosh/blob/master/doc/README.md#Bourne_Shell_Emulator)

* [Erl Shell Alternative](http://github.com/beadsland/nosh/blob/master/doc/README.md#Erl_Shell_Alternative)

* [NoSQL File System](http://github.com/beadsland/nosh/blob/master/doc/README.md#NoSQL_File_System)


 


###<a name="Installation">Installation</a>##

 


To get started with `nosh`, confirm you have an Internet connection,
  and then make the project and run the `nosh` wrapper script, as follows:
 
	
	  make install
	  bin/nosh

 


It is recommended that the project `bin/` be prefixed to your `PATH`,
  so that `nosh` can be run from any working directory.  Also included
  in `bin/` is `nano` script that detects if you're running in Wicked
  Shell (see below) and fires up an external shell to run either the
  UNIX standard `pico` editor or GNU `nano` editor where available.
  (`nano`/`pico` requires a tty to run).
 


Users of the Eclipse IDE may integrate `nosh` shell with their
  workflow by installing the Wicked Shell plugin
[`http://www.wickedshell.net/`](http://www.wickedshell.net/) from the Eclipse Marketplace.  Simply
  run `bash` or `sh` under Wicked Shell, and then execute `nosh`
from there.
 


###<a name="Bourne_Shell_Emulator">Bourne Shell Emulator</a>##

 


`nosh` emulates much of the functionality of the Bourne shell (`sh`).
 


Currently all `nosh` does is [parse](http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md) a command line
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
  on-the-fly](http://github.com/beadsland/nosh/blob/master/doc/nosh_load.md), parameters being passed (and functions called) with
minimal typing.
 


####<a name="Erl_Commands">Erl Commands</a>##

 


_Not yet implemented:_  All functions available through the Erl
  shell are available as `nosh` commands.
 


####<a name="Erlang_Context">Erlang Context</a>##

 


_Not yet implemented:_  Erlang style syntax can be used on the
`nosh` command line as per the Erl shell.  See [`nosh_parse`](http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md) for
more details.
 


###<a name="NoSQL_File_System">NoSQL File System</a>##

 
_Not yet implemented:_`nosh` will provide an NFS-style interface
  to CouchDB databases and views.
 

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/bootstrap.md" class="module">bootstrap</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh.md" class="module">nosh</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_context.md" class="module">nosh_context</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_eval.md" class="module">nosh_eval</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_exec.md" class="module">nosh_exec</a></td></tr>
<tr><td><a href="http://github.com/beadsland/nosh/blob/master/doc/nosh_parse.md" class="module">nosh_parse</a></td></tr></table>

