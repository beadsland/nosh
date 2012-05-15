

#Module noterm#

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Terminal emulator for `nosh`.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.5

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>

* <font color="red"> io:get_char (see code in jungerl)</font>
* <font color="red"> escript and parameters (make it run like any other shell command)</font>
* <font color="red"> make fully redistributable (Win/cygwin/*NIX)</font>
* <font color="red"> incorporate full terminfo/ncurses support</font>
* <font color="red"> notermd - telent/ssh access</font>
<a name="description"></a>

##Description##


 
Translates standard I/O to Erlang messaging.

_Full terminal emulation has yet to be implemented._<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start terminal, launching message loop and keyboard listening
process.</td></tr><tr><td valign="top"><a href="#start_wecho-0">start_wecho/0</a></td><td>Start terminal, with nosh echo flag set.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="start-0"></a>

###start/0##


`start() -> any()`

Start terminal, launching message loop and keyboard listening
process.<a name="start_wecho-0"></a>

###start_wecho/0##


`start_wecho() -> any()`



Start terminal, with nosh echo flag set.

This is a stopgap measure pending proper terminal emulation.