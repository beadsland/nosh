

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




###<a name="type-date_time">date_time()</a>##



<pre>date_time() = <a href="calendar.md#type-date_time">calendar:date_time()</a></pre>



###<a name="type-file_error">file_error()</a>##



<pre>file_error() = {error, {atom(), <a href="#type-filename">filename()</a>}}</pre>



###<a name="type-filename">filename()</a>##



<pre>filename() = string()</pre>



###<a name="type-format">format()</a>##



<pre>format() = <a href="io.md#type-format">io:format()</a></pre>



###<a name="type-output">output()</a>##



<pre>output() = {atom(), any()} | string()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#can_read-1">can_read/1</a></td><td>Test if file or directory is readable.</td></tr><tr><td valign="top"><a href="#can_write-1">can_write/1</a></td><td>Test if file or directory is writeable.</td></tr><tr><td valign="top"><a href="#format_erlerr-1">format_erlerr/1</a></td><td>Smartly format erlerr messages.</td></tr><tr><td valign="top"><a href="#last_modified-1">last_modified/1</a></td><td>Get last date and time file last modified.</td></tr><tr><td valign="top"><a href="#send_debug-2">send_debug/2</a></td><td>Smart DEBUG/2 macro function.</td></tr><tr><td valign="top"><a href="#send_stderr-2">send_stderr/2</a></td><td>Smart STDERR/1 macro function.</td></tr><tr><td valign="top"><a href="#send_stderr-3">send_stderr/3</a></td><td>Smart STDERR/2 macro function.</td></tr><tr><td valign="top"><a href="#send_stdout-2">send_stdout/2</a></td><td>Smart STDOUT/1 macro function.</td></tr><tr><td valign="top"><a href="#send_stdout-3">send_stdout/3</a></td><td>Smart STDOUT/2 macro function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="can_read-1"></a>

###can_read/1##




<pre>can_read(Filename::<a href="#type-filename">filename()</a>) -> boolean() | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Test if file or directory is readable.<a name="can_write-1"></a>

###can_write/1##




<pre>can_write(Filename::<a href="#type-filename">filename()</a>) -> boolean() | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Test if file or directory is writeable.<a name="format_erlerr-1"></a>

###format_erlerr/1##




<pre>format_erlerr(What::any()) -&gt; string()</pre>
<br></br>




Smartly format erlerr messages.<a name="last_modified-1"></a>

###last_modified/1##




<pre>last_modified(Filename::<a href="#type-filename">filename()</a>) -> {ok, <a href="#type-date_time">date_time()</a>} | <a href="#type-file_error">file_error()</a></pre>
<br></br>




Get last date and time file last modified.<a name="send_debug-2"></a>

###send_debug/2##




<pre>send_debug(Format::<a href="#type-format">format()</a>, What::list()) -> ok</pre>
<br></br>




Smart DEBUG/2 macro function.
Retrieves debug pid from process dictionary.  (Set by macro.)<a name="send_stderr-2"></a>

###send_stderr/2##




<pre>send_stderr(IO::#std{}, What::<a href="#type-output">output()</a>) -> ok</pre>
<br></br>




Smart STDERR/1 macro function.<a name="send_stderr-3"></a>

###send_stderr/3##




<pre>send_stderr(IO::#std{}, Format::<a href="#type-format">format()</a>, What::list()) -> ok</pre>
<br></br>




Smart STDERR/2 macro function.<a name="send_stdout-2"></a>

###send_stdout/2##




<pre>send_stdout(IO::#std{}, What::<a href="#type-output">output()</a>) -> ok</pre>
<br></br>




Smart STDOUT/1 macro function.<a name="send_stdout-3"></a>

###send_stdout/3##




<pre>send_stdout(IO::#std{}, Format::<a href="#type-format">format()</a>, What::list()) -> ok</pre>
<br></br>




Smart STDOUT/2 macro function.