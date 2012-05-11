

#Module nosh#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


The `nosh` process module, which handles messaging with a paired
`noterm` process.



Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.13

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##
 Commands:



<dt><code>hot</code></dt>



<dd>hotswap nosh modules</dd>




<dt><code>good</code></dt>



<dd>check for superly good nosh code</dd>




<dt><code>!<i>command</i></dt><dd>pass <i>command</i> to native shell</dd>
<dt>`.</code></dt>



<dd>end-of-file (exit nosh application)</dd>


 

__Draft Notes__ 

% A `nosh` process is launched by first starting [`noterm`](noterm.md), which
handles keyboard input, sending this to the `nosh` process as messages,
and presenting output and errors to the user as they are received in
message form from the `nosh` process. The `nosh` process continues to run until it receives an end-of-file
message from `noterm`.  This is currently produced by typing a period
(`.`) by itself on a line, followed by a `<newline>`.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command_run-2">command_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#hotswap_run-2">hotswap_run/2</a></td><td></td></tr><tr><td valign="top"><a href="#run-1">run/1</a></td><td>Start nosh, receiving standard I/O from noterm.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="command_run-2"></a>

###command_run/2##




`command_run(IO, Line) -> any()`

<a name="hotswap_run-2"></a>

###hotswap_run/2##




`hotswap_run(IO, Line) -> any()`

<a name="run-1"></a>

###run/1##




`run(IO) -> any()`



Start nosh, receiving standard I/O from noterm.