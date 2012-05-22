

#Module ?module#
* [Description](#description)


This is a preliminary draft of the command line execution module
for `nosh`.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.0.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red"> implicit start</font>
* <font color="red"> explicit function</font>
* <font color="red"> getoptions functions</font>
* <font color="red"> set env</font>
* <font color="red"> pattern matching</font>
* <font color="red"> Alias substitution</font>
* <font color="red"> git qualified packages</font>
<a name="description"></a>

##Description##




__Draft Notes:__



_Now thinking these multiple `start` functions will be supecededby simpler `run` functions.  Still thinking it through._



Module execution may be invoked in one of three modes:  implicit start,
explicit function or getoptions functions.



In _implicit start_ mode, the module name appears in first
position (as a command) with or without parameters.  If without
parameters, `nosh` will attempt to spawn a process running the module's
`start/4` function, or else `start/3`, as specified by the Nosh_exec
Behaviour, and failing that, will attempt to spawn on `start/0`,
reporting an error on failure.



If with parameters, `nosh` will first call any getoptions functions,
caching the results from each.  If any getoptions functions were found,
`nosh` will then attempt to spawn a process running the module's
Nosh_exec Behaviour `start/6` function, reporting an error on failure.
Otherwise, `nosh` will attempt to spawn on `start/5`, reporting an
error on failure.



In _explicit function_ mode, the command appears in Erlang syntax
as _`Module`_`:`_`Function`_.  This can stand on it's own, or
else be followed either by Bourne-style parameters or an Erlang
function parameter List.  The resulting arity function is called by
`nosh`, reporting an error on failure.  In Erlang Context, all function
invocations are in explicit function mode, always with a Parameter List.



In _getoptions functions_ mode, the module name appears in first
position, followed by parameters, including one or more 0-arity and/or
1-arity function names specified in Perl getoptions
([`http://aplawrence.com/Unix/perlgetopts.html`](http://aplawrence.com/Unix/perlgetopts.md)) syntax.  These
functions are executed in order, before `nosh` switches to implicit
start mode, as described above.



_Draft_ canonical spawn entry function:
`start(stdin, stdout, stderr, env, arg, getoptions)`



####<a name="Parameters">Parameters</a>##


Parameters may be Bourne-style words, including character sequences
delimited by single or double quotes.  In addition, Erlang-syntax data
types may be specified as parameters.  Function parameter list syntax
is a special case only valid in explicit function execution mode.