

#Module nosh#

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


The `nosh` process module, which handles messaging with a paired
`noterm` process.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.15

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##


Commands:




<dt><code>hot</code></dt>



<dd>hotswap nosh project modules</dd>




<dt><code>good</code></dt>



<dd>check for superly good nosh code</dd>




<dt><code>!<i>command</i></dt><dd>pass <i>command</i> to native shell</dd>
<dt>`.</code></dt>



<dd>end-of-file (exit nosh application)</dd>




Additionally, `nosh` will include commands from the following
subprojects:


* [Bourne shell
builtins command set](http://github.com/beadsland/nosh_bin) (nosh_bin)

* [Erl shell
alternative command set](http://github.com/beadsland/nosh_erl) (nosh_erl)

* NoSQL file system command set (nosh_nosql)





__Draft Notes__



% A `nosh` process is launched by first starting [`noterm`](noterm.md), which
handles keyboard input, sending this to the `nosh` process as messages,
and presenting output and errors to the user as they are received in
message form from the `nosh` process.

The `nosh` process continues to run until it receives an end-of-file
message from `noterm`.  This is currently produced by typing a period
(`.`) by itself on a line, followed by a `<newline>`.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command_run-2">command_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="command_run-2"></a>

###command_run/2##


`command_run(IO, Line) -> any()`

<a name="run-3"></a>

###run/3##


	run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()
<br></br>


<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.