

#Module nosh_load#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This is a preliminary draft of the module loader for `nosh`.



Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.2

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red"> document this module</font>
* <font color="red"> force purge option</font>
* <font color="red"> module binary service (to avoid repetitive slurps)</font>
* <font color="red"> conservative module loader</font>
<a name="description"></a>

##Description##
 

__Draft Notes:__ 

_The PATH search routine is not part of this module._ 

Each Erlang module is treated as an executable in `nosh`.  When the
name of a module appears in first position on a `nosh` command line, a
matching `.beam` file is sought on each directory on the `PATH`
environment variable, with one modification:  For each directory on
`PATH` that ends in `ebin\`, and for which the current user has write
access, `nosh` will look for a parallel `src\` directory, and if found,
search for a matching `.erl` file therein. 

If an associated `.erl` file is found, and it is newer that the `.beam`
file, or if an `.erl` file is found for which no `.beam` file appears,
the `.erl` file will be compiled to its `ebin\` directory.  If this
compilation is successful, the module will be loaded and evaluation
and execution proceeds.  Otherwise, the compiler error is written to
`stdout` and a non-zero status is returned. If no associated `.erl` file is found, the `.beam` file on the `PATH`
is loaded and evaluation and execution goes forward.  If no `.beam`
file is found, the search continues to the next directory on `PATH`,
returning an error if no `.beam` file can be found or compiled from
source before the `PATH` is exhausted.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-3">load/3</a></td><td></td></tr><tr><td valign="top"><a href="#test-1">test/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="load-3"></a>

###load/3##




`load(IO, Command, Path) -> any()`

<a name="test-1"></a>

###test/1##




`test(IO) -> any()`

