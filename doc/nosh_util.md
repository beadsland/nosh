

#Module nosh_util#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Utility function library.



Copyright (c) 2012 Beads D. Land-Trujillo

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">spec API functions</font>

<a name="types"></a>

##Data Types##




###<a name="type-output">output()</a>##



<pre>output() = {atom(), any()} | string()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_erlerr-1">format_erlerr/1</a></td><td>Smartly format erlerr messages.</td></tr><tr><td valign="top"><a href="#send_stderr-2">send_stderr/2</a></td><td>Smart STDERR/1 macro function.</td></tr><tr><td valign="top"><a href="#send_stdout-2">send_stdout/2</a></td><td>Smart STDOUT/1 macro function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format_erlerr-1"></a>

###format_erlerr/1##




<pre>format_erlerr(What::any()) -&gt; string()</pre>
<br></br>




Smartly format erlerr messages.<a name="send_stderr-2"></a>

###send_stderr/2##




<pre>send_stderr(IO::#std{}, What::<a href="#type-output">output()</a>) -> no_return()</pre>
<br></br>




Smart STDERR/1 macro function.<a name="send_stdout-2"></a>

###send_stdout/2##




<pre>send_stdout(IO::#std{}, What::<a href="#type-output">output()</a>) -> no_return()</pre>
<br></br>




Smart STDOUT/1 macro function.